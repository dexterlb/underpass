{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Minipass.Intermediate where

import qualified LambdaTypes as T
import LambdaTypes (PartialOrd, (<!), typeOf, Typed)
import Lambda

import qualified Minipass.Language as L
import           Minipass.Constants

import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import TypedLambda (TSLTerm)

import Control.Exception (throw)

data Types
    = Set SetTag
    | String
    | Num
    | List
    | Anything
    deriving (Eq)

instance PartialOrd Types where
    Set _       <! Set _      = True
    String      <! String     = True
    Num         <! Num        = True
    List        <! List       = True
    Anything    <! _          = True
    _           <! Anything   = True
    _           <! _          = False

instance Show Types where
    show Num        = "Num"
    show String     = "String"
    show List       = "List"
    show Anything   = "Any"
    show (Set t)    = "[" <> (show t) <> "]"

data SetTag = SetTag
    { osmTypes :: HashSet OsmType }
    deriving (Eq)

instance Show SetTag where
    show (SetTag { osmTypes }) = concatMap f $ HS.toList osmTypes
        where
            f OsmNode       = "n"
            f OsmWay        = "w"
            f OsmRelation   = "r"
            f OsmArea       = "a"

data OsmType
    = OsmNode
    | OsmWay
    | OsmRelation
    | OsmArea
    deriving (Show, Eq, Generic)

osmSet :: [OsmType] -> Types
osmSet types = Set (SetTag { osmTypes = HS.fromList types })

osmAll :: Types
osmAll = osmSet [OsmNode, OsmWay, OsmRelation, OsmArea]

meetSetTags :: SetTag -> SetTag -> SetTag
meetSetTags = intersectSetTags

intersectSetTags :: SetTag -> SetTag -> SetTag
intersectSetTags (SetTag { osmTypes = t1 }) (SetTag { osmTypes = t2 }) = SetTag
    { osmTypes = HS.intersection t1 t2 }

uniteSetTags :: SetTag -> SetTag -> SetTag
uniteSetTags (SetTag { osmTypes = t1 }) (SetTag { osmTypes = t2 }) = SetTag
    { osmTypes = HS.union t1 t2 }

instance Hashable OsmType
instance T.MSemiLattice Types where
    Num     /\ Num      = Num
    String  /\ String   = String
    List    /\ List     = List
    (Set a) /\ (Set b)  = Set $ meetSetTags a b
    x       /\ y        = throw $ T.CannotMeet x y


type Term = LambdaTerm Types Constants
type TTerm = TSLTerm Types Constants
type TTypes = T.ApplicativeType Types

instance Typed Constants Types where
    typeOf = (T.transform typeToIntermediate) . typeOf

toIntermediate :: L.Term -> Term
toIntermediate = transform Constant typeToIntermediate

typeToIntermediate :: L.Types -> Types
typeToIntermediate L.String = String
typeToIntermediate L.Num = Num
typeToIntermediate L.Set = osmAll
typeToIntermediate L.List = List
typeToIntermediate L.Anything = Anything
