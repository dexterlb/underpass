{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}

module Minipass.Language.Constants where

import Utils.Parsing as P
import Data.Functor (($>))

import           Data.Aeson (ToJSON(..))
import Utils.Latex
import Data.Text (Text, pack, unpack)

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

    deriving (Eq, Generic)

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

instance Show Constants where
    show (StringLiteral s) = "'" <> unpack s <> "'"
    show (NumLiteral    n) = show n
    show Or                = "or"
    show And               = "and"
    show Not               = "not"
    show Get               = "get"
    show Next              = "next"
    show Empty             = "empty"
    show ConsNum           = "consNum"
    show ConsString        = "consString"
    show ConsList          = "consList"

instance Latexable Constants where
    latex = pack . show

instance ToJSON Constants where
    toJSON = toJSON . show
