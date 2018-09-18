{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Minipass.Intermediate where

import Data.Functor (($>))

import qualified Parsing as P

import qualified LambdaTypes as T
import LambdaTypes (unify)
import Lambda

import qualified Minipass.Language as L

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Constants = StringLiteral Text
               | NumLiteral    Float

               | Everything
               | Nodes
               | Ways
               | Relations
               | Areas

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

    deriving (Show, Eq)

data Types
    = Set SetTag
    | String
    | Num
    | Anything

instance Eq Types where
    Set _       == Set _      = True
    String      == String     = True
    Num         == Num        = True
    Anything    == _          = True
    _           == Anything   = True
    _           == _          = False

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
unifySetTags (SetTag { osmTypes = t1 }) (SetTag { osmTypes = t2 }) = SetTag
    { osmTypes = HS.intersection t1 t2 }

instance Hashable OsmType
instance T.Unifiable Types where
    anything = T.Basic Anything
    unify x y
        | (T.TypeError _) <- z = unify' y x
        | otherwise            = z
        where
            z = unify' x y
            unify' (T.Basic Anything) x = x
            unify' (T.Application a1 a2) (T.Application b1 b2) = T.Application (unify a1 a2) (unify b1 b2)
            unify' (T.Basic Num) (T.Basic Num) = T.Basic Num
            unify' (T.Basic String) (T.Basic String) = T.Basic String
            unify' (T.Basic (Set a)) (T.Basic (Set b)) = T.Basic (Set (unifySetTags a b))
            unify' x y = T.TypeError $ "cannot unify " <> (Text.pack $ show x) <> " and " <> (Text.pack $ show y)


type Term = LambdaTerm Types Constants

instance T.Typed Constants Types where
    typeOf (StringLiteral _) = T.Basic String
    typeOf (NumLiteral    _) = T.Basic Num

    typeOf Everything        = T.Basic osmAll
    typeOf Nodes             = T.Basic $ osmSet [OsmNode]
    typeOf Ways              = T.Basic $ osmSet [OsmWay]
    typeOf Relations         = T.Basic $ osmSet [OsmRelation]
    typeOf Areas             = T.Basic $ osmSet [OsmArea]

    typeOf Kv                = T.Application (T.Basic String) (T.Application (T.Basic String) (T.Basic osmAll))
    typeOf Around            = T.Application (T.Basic Num)    (T.Application (T.Basic osmAll) (T.Basic osmAll))
    typeOf In                = T.Application (T.Basic $ osmSet [OsmArea])    (T.Basic $ osmSet [OsmNode])
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

toIntermediate :: L.Term -> Term
toIntermediate = transform constToIntermediate typeToIntermediate
    where
        typeToIntermediate L.String = String
        typeToIntermediate L.Num = Num
        typeToIntermediate L.Set = osmAll


        constToIntermediate (L.StringLiteral x) = Constant $ StringLiteral x
        constToIntermediate (L.NumLiteral    x) = Constant $ NumLiteral    x

        constToIntermediate L.Everything        = Constant $ Everything
        constToIntermediate L.Nodes             = Constant $ Nodes
        constToIntermediate L.Ways              = Constant $ Ways
        constToIntermediate L.Relations         = Constant $ Relations
        constToIntermediate L.Areas             = Constant $ Areas

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

        constToIntermediate L.Name              = Application (Constant Kv)     (Constant $ StringLiteral "name")
        constToIntermediate L.Amenity           = Application (Constant Kv)     (Constant $ StringLiteral "amenity")
        constToIntermediate L.Near              = Application (Constant Around) (Constant $ NumLiteral 50)
