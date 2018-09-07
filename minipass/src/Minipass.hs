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
    parser = (lexeme . try)
        $   P.string "foo" $> Foo
        <|> P.string "bar" $> Bar

data Types
    = Set
    | String
    deriving (Show, Eq)

instance P.Parseable Types where
    parser = (lexeme . try)
        $   P.string "Set"      $> Set
        <|> P.string "String"   $> String

type Term = LambdaTerm Types Constants

instance Typed Constants Types where
    typ Foo = T.Basic Set
    typ Bar = T.Application (T.Basic Set) (T.Basic Set)
