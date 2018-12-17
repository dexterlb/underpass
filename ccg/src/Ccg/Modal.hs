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

module Ccg.Modal where

import           GHC.Generics (Generic)
import           Data.Hashable (Hashable)
import           Data.MemoCombinators.Class (MemoTable, table)
import qualified Data.MemoCombinators as Memo
import           Data.Text (Text)
import qualified Data.Text as T

import           Ccg.Category
import           Ccg.Memoise()
import           Ccg.Latex

import           Utils.Maths

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

instance PartialOrd Modality where
    Dot     <! _        = True
    X       <! X        = True
    X       <! Star     = True
    Diamond <! Diamond  = True
    Diamond <! Star     = True
    Star    <! Star     = True
    _       <! _        = False

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
    combineBy LeftApp z (Complex (LeftSlash m) x y)
        | Just (_, r) <- y === z, m <! Star = Just $ r x
        | otherwise = Nothing
    combineBy RightApp (Complex (RightSlash m) x y) z
        | Just (r, _) <- y === z, m <! Star = Just $ r x
        | otherwise = Nothing
    combineBy _ _ _ = Nothing

-- unification

(===) :: ModalCategory -> ModalCategory -> Maybe ((ModalCategory -> ModalCategory), (ModalCategory -> ModalCategory))
(===) x y = (\(r1, r2) -> (rename r1, rename r2)) <$> unify x y

data Rename = Rename

unify :: ModalCategory -> ModalCategory -> Maybe (Rename, Rename)
unify x y
    | x == y    = Just (Rename, Rename)
    | otherwise = Nothing

rename :: Rename -> ModalCategory -> ModalCategory
rename _ = id


-- convenience functions

sc :: String -> ModalCategory
sc s = Simple $ NonTerm $ T.pack s

vc :: String -> ModalCategory
vc s = Simple $ Variable $ T.pack s

(</>) :: ModalCategory -> ModalCategory -> ModalCategory
a </> b = Complex (RightSlash Dot) a b

(<\>) :: ModalCategory -> ModalCategory -> ModalCategory
a <\> b = Complex (LeftSlash Dot) a b


