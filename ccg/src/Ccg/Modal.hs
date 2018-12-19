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
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS

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

instance MSemiLattice Modality where
    Dot  /\ x   = x
    x    /\ Dot = x
    x    /\ y
      | x == y    = x
      | otherwise = Star



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
    -- todo: make the categories have disjunct vars before combining
    combineBy rule left right
        |   LeftApp <- rule
          , z <- left', Complex (LeftSlash m) x y <- right'
          , Just (_, r) <- z === y, m <! Star
          = Just $ r x
        |   RightApp <- rule
          , Complex (RightSlash m) x y <- left', z <- right'
          , Just (r, _) <- y === z, m <! Star
          = Just $ r x
        | otherwise = Nothing
        where
            left'  = addVarSuffix "_l" left  (vars right)
            right' = addVarSuffix "_r" right (vars left)

-- unification

(===) :: ModalCategory -> ModalCategory -> Maybe ((ModalCategory -> ModalCategory), (ModalCategory -> ModalCategory))
(===) x y = (\(_, r1, r2) -> (substitute r1, substitute r2)) <$> unify x y

type Substitution = [(Text, ModalCategory)]

unify :: ModalCategory -> ModalCategory -> Maybe (ModalCategory, Substitution, Substitution)
unify (Simple (Variable a)) y = Just (y, [(a, y)], [])
unify x (Simple (Variable a)) = Just (x, [], [(a, x)])
unify (l @ (Simple (NonTerm p))) (Simple (NonTerm q))
    | p == q    = Just (l, [], [])
    | otherwise = Nothing
unify (Complex sl al bl) (Complex sr ar br) = do
    (a, alSub, arSub) <- unify al ar
    let bl' = substitute alSub bl
    let br' = substitute arSub br
    (b, blSub, brSub) <- unify bl' br'
    s <- unifySlash sl sr
    pure (Complex s a b, alSub <> blSub, arSub <> brSub)
unify _ _ = Nothing

unifySlash :: Slash -> Slash -> Maybe Slash
unifySlash (LeftSlash a)  (LeftSlash b)  = Just $ LeftSlash  $ a /\ b
unifySlash (RightSlash a) (RightSlash b) = Just $ RightSlash $ a /\ b
unifySlash _ _ = Nothing


substitute :: Substitution -> ModalCategory -> ModalCategory
substitute subs cat = foldl substituteOne cat subs

substituteOne :: ModalCategory -> (Text, ModalCategory) -> ModalCategory
substituteOne (Complex sl left right) sub =
    Complex sl (substituteOne left sub) (substituteOne right sub)
substituteOne (Simple (Variable b)) (a, x)
    | a == b    = x
    | otherwise = (Simple (Variable b))
substituteOne t _ = t

addVarSuffix :: Text -> ModalCategory -> HashSet Text -> ModalCategory
addVarSuffix suff (Complex s left right) v = (Complex s (addVarSuffix suff left  v)
                                                        (addVarSuffix suff right v))
addVarSuffix suff (Simple (Variable x))  v
    | HS.member x v = Simple $ Variable $ x <> suff
    | otherwise        = Simple $ Variable   x
addVarSuffix _ other _ = other

vars :: ModalCategory -> HashSet Text
vars (Complex _ left right) = HS.union (vars left) (vars right)
vars (Simple (Variable x))  = HS.singleton x
vars _                      = HS.empty

-- convenience functions

sc :: String -> ModalCategory
sc s = Simple $ NonTerm $ T.pack s

vc :: String -> ModalCategory
vc s = Simple $ Variable $ T.pack s

(</>) :: ModalCategory -> ModalCategory -> ModalCategory
a </> b = Complex (RightSlash Dot) a b

(<\>) :: ModalCategory -> ModalCategory -> ModalCategory
a <\> b = Complex (LeftSlash Dot) a b


