{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ccg.Rules where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.List (intercalate)
import           Data.Functor (($>))
import           Data.Aeson (ToJSON(..), object, (.=))

import           Utils.Parsing (Parseable, parser, (<|>))
import qualified Utils.Parsing as P
import           Utils.Latex (Latexable(..))


type Token = Text

type Lexer = Text -> [TokenData]

type Tag = (Text, Text) -- no need for anything fancier for now

data Rule cat payload = Rule Matcher [(cat, Constructor payload)]

data TokenData = TokenData
    { text :: Token
    , tags :: [Tag]
    } deriving (Show)

instance ToJSON TokenData where
    toJSON (TokenData { text, tags})
        = object [ "text" .= text, "tags" .= (object $ map (uncurry (.=)) tags) ]

class FromMatch payload where
    type Constructor payload

    construct :: (Constructor payload) -> TokenData -> payload


instance (Parseable cat, Parseable (Constructor payload)) => Parseable (Rule cat payload) where
    parser = P.try $ do
        matcher <- P.parser
        _       <- P.operator ":"
        targets <- P.separated "," $ do
            cat     <- P.parser
            pcons   <- P.parser
            pure (cat, pcons)
        _       <- P.operator "."
        pure $ Rule matcher targets

instance (Show cat, Show (Constructor payload)) => Show (Rule cat payload) where
    show (Rule matcher items) = show matcher <> " : " <> intercalate ", "
        (map (\(cat, cons) -> show cat <> " " <> show cons) items)

data Matcher
    = ExactMatcher  Tag
    | OrMatcher     Matcher Matcher
    | AndMatcher    Matcher Matcher
    deriving (Show)

instance Parseable Matcher where
    parser = exprParser
        where
            primitiveParser = exactParser

            exprParser = P.makeExprParser innerParser
                [ [ P.InfixL $ P.operator "|" $> OrMatcher
                  , P.InfixL $ P.operator "&" $> AndMatcher ] ]
            innerParser = P.braces exprParser <|> primitiveParser

            exactParser = P.try $ ExactMatcher <$> (do
                    tagName <- P.identifier
                    _       <- P.operator "="
                    value   <- P.quotedString '"'
                    pure $ (tagName, value)
                )

matchText :: (FromMatch payload) => Lexer -> [Rule cat payload] -> Text -> [[(cat, payload)]]
matchText lexer rules t = match rules (lexer t)

match :: (FromMatch payload) => [Rule cat payload] -> [TokenData] -> [[(cat, payload)]]
match rules = map matchToken
    where
        matchToken token = foldr (++) [] $ map (matchRuleOn token) rules
        matchRuleOn token (Rule matcher items)
            | matchRule token matcher
                = map (\(cat, cons) -> (cat, construct cons token)) items
            | otherwise = []

matchRule :: TokenData -> Matcher -> Bool
matchRule t (OrMatcher  a b) = matchRule t a || matchRule t b
matchRule t (AndMatcher a b) = matchRule t a || matchRule t b
matchRule (TokenData { tags }) (ExactMatcher tag) = elem tag tags

spaceyLexer :: Lexer
spaceyLexer = (map (\t -> TokenData { text = t, tags = [("raw", t)] })) . (Text.splitOn " ")

instance Latexable TokenData where
    latex (TokenData { tags }) = "\\minibox[t,frame]{"
        <> (Text.intercalate "\\\\" $ map (\(k, v) -> k <> ":" <> v) tags)
        <> "}"
