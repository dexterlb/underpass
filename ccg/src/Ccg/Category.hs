{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ccg.Category where

import           Data.Hashable (Hashable)
import           GHC.Generics  (Generic)
import           Data.MemoCombinators.Class (MemoTable, table)
import           Data.MemoCombinators (Memo, memo3)

import           Ccg.Latex

data Category atom slash
    = Simple  atom
    | Complex slash (Category atom slash) (Category atom slash)
    deriving (Generic)

deriving instance (Eq atom, Eq slash) => Eq (Category atom slash)
deriving instance (Hashable atom, Hashable slash) => Hashable (Category atom slash)

instance (MemoTable atom, MemoTable slash) => MemoTable (Category atom slash) where
    table = mkmemo (table :: Memo atom) (table :: Memo slash)
        where
            mkmemo :: forall atom' slash' b.
                      (forall a. (atom'  -> a) -> (atom'  -> a))
                   -> (forall a. (slash' -> a) -> (slash' -> a))
                   -> (Category atom' slash' -> b)
                   -> (Category atom' slash' -> b)
            mkmemo matom _      f (Simple x)    = (matom (f . Simple)) x
            mkmemo matom mslash f (Complex x y z) =
                (memo3 mslash mcat mcat (\a b c -> f $ Complex a b c)) x y z
                where
                    mcat :: (Category atom' slash' -> t) -> (Category atom' slash' -> t)
                    mcat = mkmemo matom mslash

instance (Show atom, Show slash) => Show (Category atom slash) where
    show (Simple atom) = show atom
    show (Complex slash a b) = "(" <> (show a) <> (show slash) <> (show b) <> ")"

instance (Latexable atom, Latexable slash) => Latexable (Category atom slash) where
    latex (Simple atom) = latex atom
    latex (Complex slash a b) = "$($" <> latex a <> latex slash <> latex b <> "$)$"

class (Finite (CombineRule a)) => Combines a where
    type CombineRule a
    combine :: a -> a -> [(CombineRule a, a)]
    combine x y = [(rule, z) | rule <- listAll
                             , Just z <- [combineBy rule x y]]

    combineBy :: CombineRule a -> a -> a -> Maybe a

class Finite a where
    listAll :: [a]

class HasPrimaryDir a where
    primaryDir :: a -> PrimaryDir

data PrimaryDir = LeftPrimary | RightPrimary | NoPrimary deriving (Eq, Show)
