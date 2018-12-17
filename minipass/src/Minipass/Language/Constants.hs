{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Minipass.Language.Constants where

import Utils.Parsing as P
import Data.Functor (($>))

import Data.Text (Text)

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

    deriving (Show, Eq)

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

