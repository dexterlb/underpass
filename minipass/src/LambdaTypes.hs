{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaTypes where

import qualified Parsing as P
import Parsing ((<|>))
import Data.Functor (($>))

import Data.Text (Text)
import qualified Data.Text as Text

data ApplicativeType b
    = Basic b
    | Application (ApplicativeType b) (ApplicativeType b)
    | TypeError Text
    | Top
    | Bottom

class (Eq b, Show b) => Typed a b | a -> b where  -- items of haskell type a have basic types from b
    typeOf :: a -> ApplicativeType b

instance (Show b) => Show (ApplicativeType b) where
    show (Basic x) = show x
    show (Application a b) = "(" <> show a <> " -> " <> show b <> ")"
    show (TypeError s) = "type_error<" <> (Text.unpack s) <> ">"

instance (Eq b) => Eq (ApplicativeType b) where
    Basic x == Basic y = x == y
    Application x y == Application p q = x == p && y == q
    _ == _ = False

instance (P.Parseable b) => P.Parseable (ApplicativeType b) where
    parser = parseTypeExpr

parseTypeExpr :: (P.Parseable b) => P.Parser (ApplicativeType b)
parseTypeExpr = P.makeExprParser parseTypeTerm
    [ [ P.InfixR (P.operator "->" $> Application) ]
    ]

parseTypeTerm :: (P.Parseable b) => P.Parser (ApplicativeType b)
parseTypeTerm
    =   P.braces parseTypeExpr
    <|> (Basic <$> P.parser)

class Show t => Unifiable t where
    unify    :: t -> t -> t     -- unify two types
    top      :: t               -- unify top x == x
    bottom   :: t               -- unify bottom x == bottom

class Show b => BasicUnifiable b where
    bunify    :: b -> b -> ApplicativeType b

instance BasicUnifiable b => Unifiable (ApplicativeType b) where
    unify (Basic x) (Basic y) = bunify x y
    unify (Application a1 a2) (Application b1 b2) = Application (unify a1 b1) (unify a2 b2)
    unify Top x = x
    unify x Top = x
    unify Bottom _ = Bottom
    unify _ Bottom = Bottom
    unify x y = TypeError $ "cannot unify " <> (Text.pack $ show x) <> " and " <> (Text.pack $ show y)

    top = Top
    bottom = Bottom


transform :: (t1 -> t2) -> ApplicativeType t1 -> ApplicativeType t2
transform f (Basic x) = Basic $ f x
transform f (Application a b) = Application (transform f a) (transform f b)
