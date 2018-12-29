{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}

module Minipass.Language.Language where

import Data.Functor (($>))

import Utils.Parsing as P
import Utils.Maths

import qualified LambdaCalculus.LambdaTypes as T
import           LambdaCalculus.LambdaTypes (Typed)
import           LambdaCalculus.Lambda

import Data.Text (Text)

import Minipass.Language.Constants

data Types
    = Set
    | String
    | Num
    | List
    | Anything
    deriving (Show, Eq)

instance PartialOrd Types where
    a <! b = a == b

instance PartialOrd (T.ApplicativeType Types) where
    (<!) = T.defaultLess

instance P.Parseable Types where
    parser = P.word
        $   P.string "Set"      $> Set
        <|> P.string "String"   $> String
        <|> P.string "Num"      $> Num

type Term = LambdaTerm Types Constants

pts :: String -> Term
pts = pss

instance Typed Constants Types where
    typeOf (StringLiteral _) = T.Basic String
    typeOf (NumLiteral    _) = T.Basic Num

    typeOf Next              = T.Application (T.Basic List) (T.Application (T.Basic Set) (T.Basic Set))
    typeOf Get               = T.Application (T.Basic List) (T.Basic Set)

    typeOf Or                = T.Application (T.Basic Set) (T.Application (T.Basic Set) (T.Basic Set))
    typeOf And               = T.Application (T.Basic Set) (T.Application (T.Basic Set) (T.Basic Set))
    typeOf Not               = T.Application (T.Basic Set) (T.Basic Set)

    typeOf ConsNum           = T.Application (T.Basic Num) (T.Application (T.Basic List) (T.Basic List))
    typeOf ConsString        = T.Application (T.Basic String) (T.Application (T.Basic List) (T.Basic List))
    typeOf ConsList          = T.Application (T.Basic List) (T.Application (T.Basic List) (T.Basic List))
    typeOf Empty             = T.Basic List


-- convenience for creating lists
data ListC
    = NumC      Float
    | StringC   Text
    | ListC     [ListC]
    deriving (Show)

listTerm :: [ListC] -> Term
listTerm []             = Constant Empty
listTerm (NumC n:xs)    = Application (Application (Constant ConsNum) (Constant $ NumLiteral n)) $ listTerm xs
listTerm (StringC n:xs) = Application (Application (Constant ConsString) (Constant $ StringLiteral n)) $ listTerm xs
listTerm (ListC n:xs)   = Application (Application (Constant ConsList) (listTerm n)) $ listTerm xs

