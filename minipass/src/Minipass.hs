{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Minipass where

import Data.Functor (($>))

import Parsing as P

import LambdaTypes as T
import Lambda

data Constants = Foo
               | Bar
    deriving (Show, Eq)

instance P.Parseable Constants where
    parser = P.word
        $   P.string "foo" $> Foo
        <|> P.string "bar" $> Bar

data Types
    = Set
    | String
    deriving (Show, Eq)

instance P.Parseable Types where
    parser = P.word
        $   P.string "Set"      $> Set
        <|> P.string "String"   $> String

type Term = LambdaTerm Types Constants

instance Typed Constants Types where
    typeOf Foo = T.Basic Set
    typeOf Bar = T.Application (T.Basic Set) (T.Basic Set)
