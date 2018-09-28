{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Minipass.Intermediate where

import qualified LambdaTypes as T
import LambdaTypes (OrderedType, (<~))
import Lambda

import qualified Minipass.Language as L

import           Data.Text (Text)
import           Context (VarName)

import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import TypedLambda (TSLTerm)

import Control.Exception (throw)

data Constants = StringLiteral Text
               | NumLiteral    Float

               | Kv
               | Around
               | In
               | Out

               | Or
               | And
               | Not

               | Up
               | Down
               | Upp
               | Downn

               | UpFilter
               | DownFilter

               | TypeFilter (HashSet OsmType)

               | FreeVar VarName

    deriving (Show, Eq)

data Types
    = Set SetTag
    | String
    | Num
    | Anything
    deriving (Eq)

instance OrderedType Types where
    Set _       <~ Set _      = True
    String      <~ String     = True
    Num         <~ Num        = True
    Anything    <~ _          = True
    _           <~ Anything   = True
    _           <~ _          = False

instance Show Types where
    show Num        = "Num"
    show String     = "String"
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

unifySetTags :: SetTag -> SetTag -> SetTag
unifySetTags = intersectSetTags

intersectSetTags :: SetTag -> SetTag -> SetTag
intersectSetTags (SetTag { osmTypes = t1 }) (SetTag { osmTypes = t2 }) = SetTag
    { osmTypes = HS.intersection t1 t2 }

uniteSetTags :: SetTag -> SetTag -> SetTag
uniteSetTags (SetTag { osmTypes = t1 }) (SetTag { osmTypes = t2 }) = SetTag
    { osmTypes = HS.union t1 t2 }

instance Hashable OsmType
instance T.Unifiable Types where
    unify Num Num = Num
    unify String String = String
    unify (Set a) (Set b) = Set $ unifySetTags a b
    unify x y = throw $ T.CannotUnify x y


type Term = LambdaTerm Types Constants
type TTerm = TSLTerm Types Constants
type TTypes = T.ApplicativeType Types

instance T.Typed Constants Types where
    typeOf (StringLiteral _) = T.Basic String
    typeOf (NumLiteral    _) = T.Basic Num

    typeOf Kv                = T.Application (T.Basic String) (T.Application (T.Basic String) (T.Basic osmAll))
    typeOf Around            = T.Application (T.Basic Num)    (T.Application (T.Basic osmAll) (T.Basic osmAll))
    typeOf In                = T.Application (T.Basic $ osmSet [OsmArea])    (T.Basic $ osmSet [OsmNode, OsmRelation, OsmWay])
    typeOf Out               = T.Application (T.Basic $ osmSet [OsmNode])    (T.Basic $ osmSet [OsmArea])

    typeOf Or                = T.Application (T.Basic osmAll) (T.Application (T.Basic osmAll) (T.Basic osmAll))
    typeOf And               = T.Application (T.Basic osmAll) (T.Application (T.Basic osmAll) (T.Basic osmAll))
    typeOf Not               = T.Application (T.Basic osmAll) (T.Basic osmAll)

    typeOf Up                = T.Application (T.Basic $ osmSet [OsmNode, OsmWay, OsmRelation]) (T.Basic $ osmSet [OsmWay, OsmRelation])
    typeOf Down              = T.Application (T.Basic $ osmSet [OsmWay, OsmRelation]) (T.Basic $ osmSet [OsmNode, OsmWay, OsmRelation])
    typeOf Upp               = T.Application (T.Basic $ osmSet [OsmNode, OsmWay, OsmRelation]) (T.Basic $ osmSet [OsmWay, OsmRelation])
    typeOf Downn             = T.Application (T.Basic $ osmSet [OsmWay, OsmRelation]) (T.Basic $ osmSet [OsmNode, OsmWay, OsmRelation])

    typeOf UpFilter          = T.Application (T.Basic String) $ T.Application (T.Basic $ osmSet [OsmNode, OsmWay, OsmRelation]) (T.Basic $ osmSet [OsmRelation])
    typeOf DownFilter        = T.Application (T.Basic String) $ T.Application (T.Basic $ osmSet [OsmRelation]) (T.Basic $ osmSet [OsmNode, OsmWay, OsmRelation])

    typeOf (TypeFilter t)    = T.Basic $ Set $ SetTag { osmTypes = t }
    typeOf (FreeVar _)       = T.Basic $ osmAll

toIntermediate :: L.Term -> Term
toIntermediate = transform constToIntermediate typeToIntermediate
    where
        typeToIntermediate L.String = String
        typeToIntermediate L.Num = Num
        typeToIntermediate L.Set = osmAll


        constToIntermediate (L.StringLiteral x) = Constant $ StringLiteral x
        constToIntermediate (L.NumLiteral    x) = Constant $ NumLiteral    x

        constToIntermediate L.Everything        = Constant $ TypeFilter $ HS.fromList [OsmNode, OsmWay, OsmRelation, OsmArea]
        constToIntermediate L.Nodes             = Constant $ TypeFilter $ HS.singleton OsmNode
        constToIntermediate L.Ways              = Constant $ TypeFilter $ HS.singleton OsmWay
        constToIntermediate L.Relations         = Constant $ TypeFilter $ HS.singleton OsmRelation
        constToIntermediate L.Areas             = Constant $ TypeFilter $ HS.singleton OsmArea

        constToIntermediate L.Kv                = Constant $ Kv
        constToIntermediate L.Around            = Constant $ Around
        constToIntermediate L.In                = Constant $ In
        constToIntermediate L.Out               = Constant $ Out

        constToIntermediate L.Or                = Constant $ Or
        constToIntermediate L.And               = Constant $ And
        constToIntermediate L.Not               = Constant $ Not

        constToIntermediate L.Up                = Constant $ Up
        constToIntermediate L.Down              = Constant $ Down
        constToIntermediate L.Upp               = Constant $ Upp
        constToIntermediate L.Downn             = Constant $ Downn

        constToIntermediate L.UpFilter          = Constant $ UpFilter
        constToIntermediate L.DownFilter        = Constant $ DownFilter

        constToIntermediate L.Name              =
            apply [Constant Kv, Constant $ StringLiteral "name"]

        constToIntermediate L.Amenity           = Lambda "amenity" (T.Basic String) $
            apply [Constant And,
                constToIntermediate L.Nodes,
                apply [Constant Kv, Constant $ StringLiteral "amenity", Variable 0]
            ]

        constToIntermediate L.Near              = apply [Constant Around, Constant $ NumLiteral 50]
