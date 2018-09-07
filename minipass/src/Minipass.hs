{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Minipass where

import Parsing as P

import LambdaTypes as T
import Lambda

data Constants
    = Foo
    | Bar
    deriving (Show, Eq)

instance P.Parseable Constants where
    parser = (P.string "foo") *> (pure Foo)

data Types
    = Set
    deriving (Show, Eq)

type Term = LambdaTerm Types Constants

instance Typed Constants Types where
    typ Foo = T.Basic Set
    typ Bar = T.Application (T.Basic Set) (T.Basic Set)