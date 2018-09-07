{-# LANGUAGE OverloadedStrings #-}

module Parsing
    ( module Text.Megaparsec
    , module Text.Megaparsec.Char
    , module Text.Megaparsec.Expr
    , Parseable, Parser, parser
    , lexeme, symbol, lambda, braces, curlyBraces
    , operator, word
    , ps
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec () Text

class Parseable t where
    parser :: Parser t

instance ShowErrorComponent () where
    showErrorComponent _ = ""

ps :: Parseable t => Text -> t
ps t = case parse (parser <* eof) "input" t of
    Right d     -> d
    Left errors -> error $ parseErrorPretty errors

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

word :: Parser a -> Parser a
word x = (lexeme . try) (x <* notFollowedBy alphaNumChar)

operator :: Text -> Parser Text
operator = lexeme . string