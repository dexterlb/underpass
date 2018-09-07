{-# LANGUAGE OverloadedStrings #-}

module Parsing
    ( module Text.Megaparsec
    , module Text.Megaparsec.Char
    , module Text.Megaparsec.Expr
    , Parseable, Parser
    , lexeme, symbol, lambda, braces, curlyBraces
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec () Text

class Parseable t where
    parser :: Parser t


sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

lambda :: Parser Text
lambda = symbol "λ"

braces :: Parser a -> Parser a
braces = between (symbol "(") (symbol ")")

curlyBraces :: Parser a -> Parser a
curlyBraces = between (symbol "{") (symbol "}")

