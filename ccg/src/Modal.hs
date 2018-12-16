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
import           Latex

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

instance Latexable NonTerm where
    latex (NonTerm t)  = "$" <> t <> "$"
    latex (Variable t) = "$var(" <> t <> ")$"

data Rule
    = LeftApp
    | RightApp
    deriving (Eq, Generic, Show)

deriving instance (Hashable Rule)

instance (HasPrimaryDir Rule) where
    primaryDir LeftApp  = RightPrimary
    primaryDir RightApp = LeftPrimary

data Slash
    = LeftSlash  Modality
    | RightSlash Modality
    deriving (Eq, Generic)
deriving instance (Hashable Slash)

instance MemoTable Slash where
    table f (LeftSlash x) = table (f . LeftSlash) $ x
    table f (RightSlash x) = table (f . RightSlash) $ x

data Modality
    = Star
    | Diamond
    | X
    | Dot
    deriving (Eq, Enum, Generic)

instance MemoTable Modality where
    table = Memo.enum

deriving instance (Hashable Modality)

instance Show Modality where
    show Star    = "★"
    show Diamond = "◆"
    show X       = "⨯"
    show Dot     = "·"

instance Latexable Modality where
    latex Star    = "\\star"
    latex Diamond = "\\Diamond"
    latex X       = "\\times"
    latex Dot     = ""

instance Show Slash where
    show (LeftSlash  m) = "\\" <> show m
    show (RightSlash m) = "/"  <> show m

instance Latexable Slash where
    latex (LeftSlash m)  = "$\\backslash_{" <> latex m <> "}$"
    latex (RightSlash m) = "$/_{"           <> latex m <> "}$"

instance Finite Rule where
    listAll = [LeftApp, RightApp]

instance Latexable Rule where
    latex = T.pack . show

instance Combines ModalCategory where
    type CombineRule ModalCategory = Rule
    combineBy LeftApp z (Complex (LeftSlash _) x y)
        | y == z = Just x
        | otherwise = Nothing
    combineBy RightApp (Complex (RightSlash _) x y) z
        | y == z = Just x
        | otherwise = Nothing
    combineBy _ _ _ = Nothing

sc :: String -> ModalCategory
sc s = Simple $ NonTerm $ T.pack s

vc :: String -> ModalCategory
vc s = Simple $ Variable $ T.pack s

(</>) :: ModalCategory -> ModalCategory -> ModalCategory
a </> b = Complex (RightSlash Dot) a b

(<\>) :: ModalCategory -> ModalCategory -> ModalCategory
a <\> b = Complex (LeftSlash Dot) a b


