{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Minipass.Language where

import Data.Functor (($>))

import Parsing as P

import LambdaTypes as T
import Lambda

import Data.Text (Text)

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

               | Name
               | Amenity
               | Near

    deriving (Show, Eq)

instance P.Parseable Constants where
    parser = parseKeyword <|> parseStringLiteral <|> parseNumLiteral

parseStringLiteral :: P.Parser Constants
parseStringLiteral = StringLiteral <$> P.quotedString '\''

parseNumLiteral :: P.Parser Constants
parseNumLiteral = NumLiteral <$> P.floatNumber

parseKeyword :: P.Parser Constants
parseKeyword
    =   P.word "everything"  $> Everything
    <|> P.word "nodes"       $> Nodes
    <|> P.word "ways"        $> Ways
    <|> P.word "relations"   $> Relations
    <|> P.word "areas"       $> Areas

    <|> P.word "kv"          $> Kv
    <|> P.word "around"      $> Around
    <|> P.word "in"          $> In
    <|> P.word "out"         $> Out

    <|> P.word "or"          $> Or
    <|> P.word "and"         $> And
    <|> P.word "not"         $> Not

    <|> P.word "up"          $> Up
    <|> P.word "down"        $> Down
    <|> P.word "upp"         $> Upp
    <|> P.word "downn"       $> Downn

    <|> P.word "upf"         $> UpFilter
    <|> P.word "downf"       $> DownFilter

    <|> P.word "name"        $> Name
    <|> P.word "amenity"     $> Amenity
    <|> P.word "near"        $> Near

data Types
    = Set
    | String
    | Num
    deriving (Show, Eq)

instance PartialOrd Types where
    a <! b = a == b

instance P.Parseable Types where
    parser = P.word
        $   P.string "Set"      $> Set
        <|> P.string "String"   $> String
        <|> P.string "Num"      $> String

type Term = LambdaTerm Types Constants

pts :: String -> Term
pts = pss

instance Typed Constants Types where
    typeOf (StringLiteral _) = T.Basic String
    typeOf (NumLiteral    _) = T.Basic Num

    typeOf Everything        = T.Basic Set
    typeOf Nodes             = T.Basic Set
    typeOf Ways              = T.Basic Set
    typeOf Relations         = T.Basic Set
    typeOf Areas             = T.Basic Set

    typeOf Kv                = T.Application (T.Basic String) (T.Application (T.Basic String) (T.Basic Set))
    typeOf Around            = T.Application (T.Basic Num)    (T.Application (T.Basic Set)    (T.Basic Set))
    typeOf In                = T.Application (T.Basic Set)    (T.Basic Set)
    typeOf Out               = T.Application (T.Basic Set)    (T.Basic Set)

    typeOf Or                = T.Application (T.Basic Set) (T.Application (T.Basic Set) (T.Basic Set)) -- Set -> Set -> Set
    typeOf And               = T.Application (T.Basic Set) (T.Application (T.Basic Set) (T.Basic Set)) -- Set -> Set -> Set
    typeOf Not               = T.Application (T.Basic Set) (T.Basic Set) -- Set -> Set

    typeOf Up                = T.Application (T.Basic Set) (T.Basic Set) -- Set -> Set
    typeOf Down              = T.Application (T.Basic Set) (T.Basic Set) -- Set -> Set
    typeOf Upp               = T.Application (T.Basic Set) (T.Basic Set) -- Set -> Set
    typeOf Downn             = T.Application (T.Basic Set) (T.Basic Set) -- Set -> Set

    typeOf UpFilter          = T.Application (T.Basic String) (T.Application (T.Basic Set) (T.Basic Set)) -- String -> Set -> Set
    typeOf DownFilter        = T.Application (T.Basic String) (T.Application (T.Basic Set) (T.Basic Set)) -- String -> Set -> Set

    typeOf Name              = T.Application (T.Basic String) (T.Basic Set) -- String -> Set
    typeOf Amenity           = T.Application (T.Basic String) (T.Basic Set) -- String -> Set
    typeOf Near              = T.Application (T.Basic Set)    (T.Basic Set) -- Set    -> Set
