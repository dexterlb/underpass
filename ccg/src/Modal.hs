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
{-# LANGUAGE OverloadedStrings #-}

module Modal where

import           GHC.Generics (Generic)
import           Data.Hashable (Hashable)
import           Data.MemoCombinators.Class (MemoTable, table)
import qualified Data.MemoCombinators as Memo
import           Data.Text (Text)
import qualified Data.Text as T

import           Category
import           Memoise()

type ModalCategory = Category NonTerm Slash

data NonTerm
    = NonTerm   Text
    | Variable  Text
    deriving (Eq, Generic)

instance MemoTable NonTerm where
    table f (NonTerm  x) = (table (f . NonTerm)) x
    table f (Variable x) = (table (f . Variable)) x

deriving instance (Hashable NonTerm)
instance Show NonTerm where
    show (NonTerm  t) = T.unpack t
    show (Variable t) = T.unpack $ "<" <> t <> ">"

data Rule
    = LeftApp
    | RightApp
    deriving (Eq, Generic, Show)

deriving instance (Hashable Rule)

data Slash
    = LeftSlash
    | RightSlash
    deriving (Eq, Generic, Enum)
deriving instance (Hashable Slash)

instance MemoTable Slash where
    table = Memo.enum

instance Show Slash where
    show LeftSlash  = "\\"
    show RightSlash = "/"

instance Finite Rule where
    listAll = [LeftApp, RightApp]

instance Combines ModalCategory where
    type CombineRule ModalCategory = Rule
    combineBy LeftApp z (Complex LeftSlash x y)
        | y == z = Just x
        | otherwise = Nothing
    combineBy RightApp (Complex RightSlash x y) z
        | y == z = Just x
        | otherwise = Nothing
    combineBy _ _ _ = Nothing

sc :: String -> ModalCategory
sc s = Simple $ NonTerm $ T.pack s

vc :: String -> ModalCategory
vc s = Simple $ Variable $ T.pack s

(</>) :: ModalCategory -> ModalCategory -> ModalCategory
a </> b = Complex RightSlash a b

(<\>) :: ModalCategory -> ModalCategory -> ModalCategory
a <\> b = Complex LeftSlash a b


