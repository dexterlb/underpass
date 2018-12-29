{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Ccg.TypeSystem where

import           LambdaCalculus.LambdaTypes (ApplicativeType(..), TypeException(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Data.Hashable (Hashable, hashWithSalt)
import           Control.Exception (throw)
import           Data.MemoCombinators.Class (MemoTable, table)
import           Data.MemoCombinators (Memo, memo2)

import           Utils.Maths

import           Ccg.Latex
import           Ccg.Memoise ()

type Name = Text

-- The following types define a wrapping typesystem which extends the
-- given typesystem `b`. The new types have simple text names and extend
-- respective applicative types from the typesystem `b`.


data TypeWrapper b
    = Type    b
    | SubType Name (AppTypeWrapper b)
    deriving (Generic)

type AppTypeWrapper b = ApplicativeType (TypeWrapper b)

instance Eq b => Eq (TypeWrapper b) where
    (Type x)      == (Type y)      = x == y
    (SubType p _) == (SubType q _) = p == q
    _             == _             = False

instance forall b. (Hashable b) => Hashable (TypeWrapper b) where
    hashWithSalt salt (Type x)         = hashWithSalt salt (Left x     :: Either b Name)
    hashWithSalt salt (SubType name _) = hashWithSalt salt (Right name :: Either b Name)

instance (Eq b, PartialOrd b) => PartialOrd (ApplicativeType (TypeWrapper b)) where
    (<!) (Basic (Type x)) (Basic (Type y))   = x <! y
    (<!) (Application a b) (Application c d) = ((<!) a c) && ((<!) b d)
    (<!) (Basic (SubType x pa)) (r @ (Basic (SubType y _)))
        | x == y = True
        | otherwise = (<!) pa r
    (<!) (Basic (SubType _ pa)) (r @ (Basic (Type  _))) = (<!) pa r
    (<!) (Basic (SubType _ pa)) (r @ (Application _ _)) = (<!) pa r
    (<!) Bot    Bot          = True
    (<!) Bot    _            = False
    (<!) _      Bot          = True
    (<!) _      _            = False


instance (Eq b, MSemiLattice b) => MSemiLattice (ApplicativeType (TypeWrapper b)) where
    (/\) (Basic (Type x)) (Basic (Type y)) = Basic $ Type $ x /\ y
    (/\) (Application a b) (Application c d) = Application (a /\ c) (b /\ d)
    (/\) x y
        | x <! y = x
        | x !> y = y
        | otherwise    = throw $ CannotMeet x y

-- memo instances
instance (MemoTable t) => MemoTable (TypeWrapper t) where
    table = mkmemo (table :: Memo t) (table :: Memo (AppTypeWrapper t))
        where
            mkmemo :: forall t' b.
                      (forall a. (t'               -> a) -> (t'               -> a))
                   -> (forall a. ((AppTypeWrapper t') -> a) -> ((AppTypeWrapper t') -> a))
                   -> (TypeWrapper t' -> b)
                   -> (TypeWrapper t' -> b)
            mkmemo mt _   f (Type      x) = mt (f . Type) x
            mkmemo _  mwt f (SubType x y) = memo2 mname mwt (\a b -> f $ SubType a b) x y
                where
                    mname = table :: Memo Name

-- shows and latexes
instance (Latexable b) => Latexable (TypeWrapper b) where
    latex (Type b) = latex b
    latex (SubType name _) = name

instance (Show b) => Show (TypeWrapper b) where
    show (Type b) = show b
    show (SubType name _) = T.unpack name
