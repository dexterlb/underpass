{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ccg.Sandbox where

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Data.Hashable (Hashable)
import           Data.MemoCombinators.Class (MemoTable, table)
import qualified Data.MemoCombinators as Memo

import           Utils.Maths

import           Ccg.Category
import           Ccg.Trees
import           Ccg.Cyk
import           Ccg.Modal
import           Ccg.Latex

data StupidType
    = S | A | B | C | N
    deriving (Show, Eq, Generic, Enum)

deriving instance Hashable StupidType

instance PartialOrd StupidType where
    a <! b = a == b

instance MSemiLattice StupidType where
    a /\ b
        | a == b = a
        | otherwise = error "trying to meet incompatible stupid types"

instance MemoTable StupidType where
    table = Memo.enum

instance Latexable StupidType where
    latex = T.pack . show

simpleWord :: Vector [(ModalCategory StupidType, Text)]
simpleWord = V.fromList
    [ [(sc A, "a"), (sc S </> sc C </> sc B, "a")]
    , [(sc S </> sc C <\> sc A, "b"), (sc B, "b"), (sc S </> vc "p" <\> vc "p", "b")]
    , [(sc C, "c"), (sc A, "c")]
    ]

simpleCyk :: [ParseTree (ModalCategory StupidType) Text]
simpleCyk = enumTrees $ cyk simpleWord $ Simple $ NonTerm S

latexCyk :: IO ()
latexCyk = latexPreview simpleCyk
