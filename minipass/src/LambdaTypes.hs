{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module LambdaTypes where

import qualified Parsing as P
import Parsing ((<|>))
import Data.Functor (($>))

import Control.Exception (Exception, throw)
import Data.Dynamic (Typeable)

data ApplicativeType b
    = Basic b
    | Application (ApplicativeType b) (ApplicativeType b)
    | Top
    | Bottom

class (Show b, Show a, Typeable a, Typeable b, OrderedType b) => Typed a b | a -> b where  -- items of haskell type a have basic types from b
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

class (Show t, Typeable t, OrderedType t) => Unifiable t where
    unify    :: t -> t -> t     -- unify two types

class (Show t) => HasTop t where
    top      :: t               -- unify top x == x

instance Unifiable b => Unifiable (ApplicativeType b) where
    unify (Basic x) (Basic y) = Basic $ unify x y
    unify (Application a1 a2) (Application b1 b2) = Application (unify a1 b1) (unify a2 b2)
    unify Top x = x
    unify x Top = x
    unify Bottom _ = Bottom
    unify _ Bottom = Bottom
    unify x y = throw $ CannotUnify x y

instance (Show b) => HasTop (ApplicativeType b) where
    top = Top

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

data TypeException t
    = CannotUnify t t
    | WrongLambdaType t
    deriving (Typeable)

deriving instance Show t => Show (TypeException t)

instance (Show t, Typeable t) => Exception (TypeException t)
