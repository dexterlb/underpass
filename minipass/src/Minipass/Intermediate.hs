{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

module Minipass.Intermediate where

import Data.Functor (($>))

import Parsing as P

import LambdaTypes as T
import Lambda

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
    deriving (Show, Eq)

data SetTag = SetTag
    { osmTypes :: HashSet OsmType }
    deriving (Show, Eq)

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

instance Hashable OsmType

type Term = LambdaTerm Types Constants

instance Typed Constants Types where
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