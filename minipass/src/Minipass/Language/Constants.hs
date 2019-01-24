{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}

module Minipass.Language.Constants where

import Utils.Parsing as P
import Data.Functor (($>))

import Utils.Latex
import Data.Text (Text, pack)

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

data Constants = StringLiteral Text
               | NumLiteral    Float

               | Or
               | And
               | Not

               | Get
               | Next

               | Empty
               | ConsNum
               | ConsString
               | ConsList

    deriving (Show, Eq, Generic)

instance Hashable Constants

instance P.Parseable Constants where
    parser = parseKeyword <|> parseStringLiteral <|> parseNumLiteral

parseStringLiteral :: P.Parser Constants
parseStringLiteral = StringLiteral <$> P.quotedString '\''

parseNumLiteral :: P.Parser Constants
parseNumLiteral = NumLiteral <$> P.floatNumber

parseKeyword :: P.Parser Constants
parseKeyword
    =   P.word "or"          $> Or
    <|> P.word "and"         $> And
    <|> P.word "not"         $> Not

    <|> P.word "get"         $> Get
    <|> P.word "next"        $> Next

    <|> P.word "consNum"     $> ConsNum
    <|> P.word "consString"  $> ConsString
    <|> P.word "consList"    $> ConsList
    <|> P.word "empty"       $> Empty

instance Latexable Constants where
    latex = pack . show
