{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Category where

import           Data.Hashable (Hashable)
import           GHC.Generics  (Generic)
import           Data.MemoCombinators.Class (MemoTable, table)
import           Data.MemoCombinators (Memo, memo3)

data Category atom slash
    = Atom  atom
    | Slash slash (Category atom slash) (Category atom slash)
    deriving (Generic)

deriving instance (Eq atom, Eq slash) => Eq (Category atom slash)
deriving instance (Hashable atom, Hashable slash) => Hashable (Category atom slash)

instance (MemoTable atom, MemoTable slash) => MemoTable (Category atom slash) where
    table = mkmemo (table :: Memo atom) (table :: Memo slash)

mkmemo :: forall atom slash b.
          (forall a. (atom -> a)  -> (atom -> a))
       -> (forall a. (slash -> a) -> (slash -> a))
       -> (Category atom slash -> b)
       -> (Category atom slash -> b)
mkmemo matom _      f (Atom x) = (matom (f . Atom)) x
mkmemo matom mslash f (Slash x y z) = (memo3 mslash mcat mcat (\a b c -> f $ Slash a b c)) x y z
    where
        mcat :: (Category atom slash -> t) -> (Category atom slash -> t)
        mcat = mkmemo matom mslash

instance (Show atom, Show slash) => Show (Category atom slash) where
    show (Atom atom) = show atom
    show (Slash slash a b) = "(" <> (show a) <> (show slash) <> (show b) <> ")"

class (Finite (Rule a)) => Combines a where
    type Rule a
    combine :: a -> a -> [(Rule a, a)]
    combine x y = [(rule, z) | rule <- listAll
                             , Just z <- [combineBy rule x y]]

    combineBy :: Rule a -> a -> a -> Maybe a

class Finite a where
    listAll :: [a]
