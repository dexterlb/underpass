{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaTypes where

import qualified Parsing as P
import Parsing ((<|>))
import Data.Functor (($>))

import Data.Text (Text)
import qualified Data.Text as T

data ApplicativeType b
    = Basic b
    | Application (ApplicativeType b) (ApplicativeType b)
    | TypeError Text

class (Eq b, Show b) => Typed a b | a -> b where  -- items of haskell type a have basic types from b
    typeOf :: a -> ApplicativeType b

instance (Show b) => Show (ApplicativeType b) where
    show (Basic x) = show x
    show (Application a b) = "(" <> show a <> " -> " <> show b <> ")"
    show (TypeError s) = "type_error<" <> (T.unpack s) <> ">"

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

class Unifiable t where
    unify    :: ApplicativeType t -> ApplicativeType t -> ApplicativeType t
    anything :: ApplicativeType t

instance Unifiable t => Semigroup (ApplicativeType t) where
    (<>) = unify

instance Unifiable t => Monoid (ApplicativeType t) where
    mempty  = anything

transform :: (t1 -> t2) -> ApplicativeType t1 -> ApplicativeType t2
transform f (Basic x) = Basic $ f x
transform f (Application a b) = Application (transform f a) (transform f b)