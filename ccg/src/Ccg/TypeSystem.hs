{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

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

data TypeBox b = TypeBox Bool (WrappedType b) deriving (Generic) -- todo: add features here

deriving instance (Eq b) => Eq (TypeBox b)
deriving instance (Hashable b) => Hashable (TypeBox b)

type WrappedType b = ApplicativeType (TypeWrapper b)

data TypeWrapper b
    = Type      b
    | SubType   Name (WrappedType b)
    deriving (Generic)

data HashTypeWrapper b
    = HashType b
    | HashSubType Name
    deriving (Generic)

deriving instance (Hashable b) => Hashable (HashTypeWrapper b)

instance Eq b => Eq (TypeWrapper b) where
    (Type x)      == (Type y)      = x == y
    (SubType p _) == (SubType q _) = p == q
    _             == _             = False

instance forall b. (Hashable b) => Hashable (TypeWrapper b) where
    hashWithSalt salt (Type x) = hashWithSalt salt (HashType x)
    hashWithSalt salt (SubType name _) = hashWithSalt salt ((HashSubType name) :: HashTypeWrapper b)

instance (Eq b, PartialOrd b) => PartialOrd (TypeBox b) where
    (TypeBox _ a) <! (TypeBox True b) = lessThan a b
    (TypeBox _ a) <! (TypeBox _    b) = a == b

instance (Eq b, MSemiLattice b) => MSemiLattice (TypeBox b) where
    (TypeBox ba a) /\ (TypeBox bb b) = TypeBox (ba && bb) (meet a b)

lessThan :: (Eq b, PartialOrd b) => WrappedType b -> WrappedType b -> Bool
lessThan (Basic (Type x)) (Basic (Type y))   = x <! y
lessThan (Application a b) (Application c d) = (lessThan a c) && (lessThan b d)
lessThan (Basic (SubType x pa)) (r @ (Basic (SubType y _)))
    | x == y = True
    | otherwise = lessThan pa r
lessThan (Basic (SubType _ pa)) (r @ (Basic (Type  _))) = lessThan pa r
lessThan (Basic (SubType _ pa)) (r @ (Application _ _)) = lessThan pa r
lessThan Bot    Bot          = True
lessThan Bot    _            = False
lessThan _      Bot          = True
lessThan _      _            = False

meet :: (Eq b, MSemiLattice b) => WrappedType b -> WrappedType b -> WrappedType b
meet (Basic (Type x)) (Basic (Type y)) = Basic $ Type $ x /\ y
meet (Application a b) (Application c d) = Application (a `meet` c) (b `meet` d)
meet x y
    | lessThan x y = x
    | lessThan y x = y
    | otherwise    = throw $ CannotMeet x y

-- memo instances
instance (MemoTable t) => MemoTable (TypeBox t) where
    table = mkmemo (table :: Memo Bool) (table :: Memo (WrappedType t))
        where
            mkmemo :: forall t' b.
                      (forall a. (Bool             -> a) -> (Bool             -> a))
                   -> (forall a. ((WrappedType t') -> a) -> ((WrappedType t') -> a))
                   -> (TypeBox t' -> b)
                   -> (TypeBox t' -> b)
            mkmemo mbool mt f (TypeBox x y) = memo2 mbool mt (\a b -> f $ TypeBox a b) x y

instance (MemoTable t) => MemoTable (TypeWrapper t) where
    table = mkmemo (table :: Memo t) (table :: Memo (WrappedType t))
        where
            mkmemo :: forall t' b.
                      (forall a. (t'               -> a) -> (t'               -> a))
                   -> (forall a. ((WrappedType t') -> a) -> ((WrappedType t') -> a))
                   -> (TypeWrapper t' -> b)
                   -> (TypeWrapper t' -> b)
            mkmemo mt _   f (Type      x) = mt (f . Type) x
            mkmemo _  mwt f (SubType x y) = memo2 mname mwt (\a b -> f $ SubType a b) x y
                where
                    mname = table :: Memo Name

-- shows and latexes
instance (Latexable b) => Latexable (TypeBox b) where
    latex (TypeBox True  x) = "any " <> latex x
    latex (TypeBox False x) =           latex x

instance (Show b) => Show (TypeBox b) where
    show (TypeBox True  x) = "any " <> show x
    show (TypeBox False x) =           show x

instance (Latexable b) => Latexable (TypeWrapper b) where
    latex (Type b) = latex b
    latex (SubType name _) = name

instance (Show b) => Show (TypeWrapper b) where
    show (Type b) = show b
    show (SubType name _) = T.unpack name
