{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

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
import           Utils.Latex
import           LambdaCalculus.UserTypeSystem

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

instance MLattice StupidType where
    a \/ b
        | a == b = a
        | otherwise = error "trying to join incompatible stupid types"

instance PartialOrd (T.ApplicativeType StupidType) where
    (<!) = T.defaultLess

instance MSemiLattice (T.ApplicativeType StupidType) where
    (/\) = T.defaultMeet

instance MLattice (T.ApplicativeType StupidType) where
    (\/) = T.defaultJoin


instance MemoTable StupidType where
    table = Memo.enum

instance Latexable StupidType where
    latex = T.pack . show

type Cat = ModalCategory (AppTypeWrapper StupidType)

simpleWord :: Vector [(Cat, Text)]
simpleWord = V.fromList
    [ [(bc A, "a"), (bc S </> bc C </> bc B, "a")]
--    [ [(sc $ T.Basic $ SubType "AA" $ T.Basic $ Type $ A, "a"), (bc S </> bc C </> bc B, "a")]
    , [(bc S </> bc C <\> bcAny A, "b"), (bc B, "b"), (bc S </> vc "p" <\> vc "p", "b")]
    , [(bc C, "c"), (bc A, "c")]
    ]

bc :: StupidType -> Cat
bc = sc . T.Basic . Type

bcAny :: StupidType -> Cat
bcAny = sc . T.Basic . Type

simpleCyk :: [ParseTree (Cat) Text]
simpleCyk = enumTrees $ cyk simpleWord $ bc S

latexCyk :: IO ()
latexCyk = latexPreview simpleCyk
