{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaTypes where

import qualified Parsing as P
import Parsing ((<|>))
import Data.Functor (($>))

import Control.Monad (liftM2)
import Control.Monad.Fail (MonadFail)

data ApplicativeType b
    = Basic b
    | Application (ApplicativeType b) (ApplicativeType b)
    | Top
    | Bottom

class (Show b, OrderedType b) => Typed a b | a -> b where  -- items of haskell type a have basic types from b
    typeOf :: a -> ApplicativeType b

instance (Show b) => Show (ApplicativeType b) where
    show (Basic x) = show x
    show (Application a b) = "(" <> show a <> " -> " <> show b <> ")"
    show Top = "⊤"
    show Bottom = "⊥"

instance (Eq b) => Eq (ApplicativeType b) where
    Basic x == Basic y = x == y
    Application x y == Application p q = x == p && y == q
    Top == _ = True
    _ == Top = True
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

class OrderedType t where
    (<~)      :: t -> t -> Bool

class (Show t, OrderedType t) => Unifiable t where
    unify    :: MonadFail m => t -> t -> m t   -- unify two types
    top      :: t                   -- unify top x == x
    bottom   :: t                   -- unify bottom x == bottom

instance Unifiable b => Unifiable (ApplicativeType b) where
    unify (Basic x) (Basic y) = Basic <$> unify x y
    unify (Application a1 a2) (Application b1 b2) = liftM2 Application (unify a1 b1) (unify a2 b2)
    unify Top x = pure x
    unify x Top = pure x
    unify Bottom _ = pure bottom
    unify _ Bottom = pure bottom
    unify x y      = fail $ "cannot unify " <> show x <> " and " <> show y

    top = Top
    bottom = Bottom

instance OrderedType b => OrderedType (ApplicativeType b) where
    (<~) (Basic x)           (Basic y)           = (<~) x y
    (<~) (Application a1 a2) (Application b1 b2) = ((<~) a1 b1) && ((<~) a2 b2)
    (<~) Top    Top          = True
    (<~) Top    _            = False
    (<~) _      Top          = True
    (<~) Bottom Bottom       = True
    (<~) Bottom _            = True
    (<~) _      Bottom       = False
    (<~) _      _            = False

transform :: (t1 -> t2) -> ApplicativeType t1 -> ApplicativeType t2
transform f (Basic x)           = Basic $ f x
transform f (Application a b)   = Application (transform f a) (transform f b)
transform _ Top                 = Top
transform _ Bottom              = Bottom
