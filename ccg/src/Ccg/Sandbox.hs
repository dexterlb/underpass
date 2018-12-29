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
import qualified LambdaCalculus.LambdaTypes as T

import           Ccg.Trees
import           Ccg.Cyk
import           Ccg.Modal
import           Ccg.Latex
import           Ccg.TypeSystem

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

type Cat = ModalCategory (TypeBox StupidType)

simpleWord :: Vector [(Cat, Text)]
simpleWord = V.fromList
    [ [(bc A, "a"), (bc S </> bc C </> bc B, "a")]
    , [(bc S </> bc C <\> bc A, "b"), (bc B, "b"), (bc S </> vc "p" <\> vc "p", "b")]
    , [(bc C, "c"), (bc A, "c")]
    ]

bc :: StupidType -> Cat
bc = sc . (TypeBox False) . T.Basic . Type

simpleCyk :: [ParseTree (Cat) Text]
simpleCyk = enumTrees $ cyk simpleWord $ bc S

latexCyk :: IO ()
latexCyk = latexPreview simpleCyk
