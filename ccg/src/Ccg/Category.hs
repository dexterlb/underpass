{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ccg.Category where

import           Data.Hashable (Hashable)
import           GHC.Generics  (Generic)
import           Data.MemoCombinators.Class (MemoTable, table)
import           Data.MemoCombinators (Memo, memo3)
import           Data.Dynamic (Typeable)

import qualified LambdaCalculus.LambdaTypes as T
import           LambdaCalculus.LambdaTypes (Typed, typeOf)

import           Utils.Latex
import           Utils.Parsing (Parseable(..), (<|>))
import qualified Utils.Parsing as P

data Category atom slash
    = Simple  atom
    | Complex slash (Category atom slash) (Category atom slash)
    deriving (Eq, Hashable, Generic)

instance (MemoTable atom, MemoTable slash) => MemoTable (Category atom slash) where
    table = mkmemo (table :: Memo atom) (table :: Memo slash)
        where
            mkmemo :: forall atom' slash' b.
                      (forall a. (atom'  -> a) -> (atom'  -> a))
                   -> (forall a. (slash' -> a) -> (slash' -> a))
                   -> (Category atom' slash' -> b)
                   -> (Category atom' slash' -> b)
            mkmemo matom _      f (Simple x)    = matom (f . Simple) x
            mkmemo matom mslash f (Complex x y z) =
                memo3 mslash mcat mcat (\a b c -> f $ Complex a b c) x y z
                where
                    mcat :: (Category atom' slash' -> t) -> (Category atom' slash' -> t)
                    mcat = mkmemo matom mslash

instance (Show atom, Show slash) => Show (Category atom slash) where
    show (Simple atom) = show atom
    show (Complex slash a b) = "(" <> show a <> show slash <> show b <> ")"

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

cmap :: (atom1 -> atom2) -> Category atom1 slash -> Category atom2 slash
cmap f (Simple atom)         = Simple $ f atom
cmap f (Complex slash c1 c2) = Complex slash (cmap f c1) (cmap f c2)

-- parsing
instance (Parseable atom, Parseable slash) => Parseable (Category atom slash) where
    parser = parseCatExpr
        where
            parseCatExpr = P.makeExprParser parseCatTerm
                -- the following is so elegant that it will take you 2 weeks to read
                [ [ P.InfixL (Complex <$> parser) ] ]

            parseCatTerm
                =   P.braces parseCatExpr
                <|> (Simple <$> parser)

-- lambda instances (maybe they shouldn't be here?)
instance (Show payload, Typeable payload,  Typed atom t) => Typed (Category atom payload) t where
    typeOf (Simple a) = typeOf a
    typeOf (Complex _ l r) = T.Application (typeOf r) (typeOf l)
