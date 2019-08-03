{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ccg.Simple where

import           GHC.Generics (Generic)
import           Data.Hashable (Hashable)
import           Data.MemoCombinators.Class (MemoTable, table)
import qualified Data.MemoCombinators as Memo

import           Ccg.Category

type SimpleCategory = Category String SimpleSlash

data SimpleRule
    = LeftApp
    | RightApp
    deriving (Eq, Generic, Show)

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
    type CombineRule SimpleCategory = SimpleRule
    combineBy LeftApp z (Complex LeftSlash x y)
        | y == z = Just x
        | otherwise = Nothing
    combineBy RightApp (Complex RightSlash x y) z
        | y == z = Just x
        | otherwise = Nothing
    combineBy _ _ _ = Nothing
