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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Simple where

import           GHC.Generics (Generic)
import           Data.Hashable (Hashable)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.MemoCombinators.Class (MemoTable, table)
import qualified Data.MemoCombinators as Memo

import           Category
import           Trees
import           Cyk

type SimpleCategory = Category String SimpleSlash

data SimpleRule
    = LeftApp
    | RightApp
    deriving (Eq, Generic)

deriving instance (Hashable SimpleRule)

data SimpleSlash
    = LeftSlash
    | RightSlash
    deriving (Eq, Generic, Enum)

deriving instance (Hashable SimpleSlash)

instance MemoTable SimpleSlash where
    table = Memo.enum

instance Show SimpleSlash where
    show LeftSlash  = "\\"
    show RightSlash = "/"

instance Finite SimpleRule where
    listAll = [LeftApp, RightApp]

instance Combines SimpleCategory where
    type Rule SimpleCategory = SimpleRule
    combineBy LeftApp (Slash LeftSlash x y) z
        | x == z = Just y
        | otherwise = Nothing
    combineBy RightApp (Slash RightSlash x y) z
        | y == z = Just x
        | otherwise = Nothing
    combineBy _ _ _ = Nothing

simpleWord :: Vector [(SimpleCategory, String)]
simpleWord = V.fromList
    [ [(Atom "A", "a")]
    , [(Slash LeftSlash (Slash RightSlash (Atom "S") (Atom "C")) (Atom "A"), "b" )]
    , [(Atom "C", "c")]
    ]

simpleCyk :: [ParseTree SimpleCategory String]
simpleCyk = enumTrees $ cyk simpleWord $ Atom "S"
