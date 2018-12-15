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

module Category where

import           Data.Hashable (Hashable)
import           GHC.Generics  (Generic)
import           Data.MemoCombinators.Class (MemoTable, table)
import qualified Data.MemoCombinators as Memo

data Category atom slash
    = Atom  atom
    | Slash slash (Category atom slash) (Category atom slash)
    deriving (Generic)

deriving instance (Eq atom, Eq slash) => Eq (Category atom slash)
deriving instance (Hashable atom, Hashable slash) => Hashable (Category atom slash)


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
