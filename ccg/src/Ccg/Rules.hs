{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ccg.Rules where

import           Data.Text (Text)
import           Data.List (intercalate)

import           Utils.Parsing (Parseable, parser)
import qualified Utils.Parsing as P

type Token = Text

type Lexer = Text -> [TokenData]

data Rule cat payload = Rule Matcher [(cat, Constructor payload)]

data TokenData = TokenData
    { text :: Token
    }

data MatchData = MatchData
    { token :: Token
    }

class FromMatch payload where
    type Constructor payload

    construct :: (Constructor payload) -> MatchData -> payload


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
    = ExactMatcher Token
    deriving (Show)

instance Parseable Matcher where
    parser = exactMatcherParser
        where
            exactMatcherParser = P.try $ ExactMatcher <$> P.quotedString '"'

match :: (FromMatch payload) => [Rule cat payload] -> [TokenData] -> [[(cat, payload)]]
match rules = map matchToken
    where
        matchToken token = foldr (++) [] $ map (matchRuleOn token) rules
        matchRuleOn token (Rule matcher items)
            | (Just matchData) <- matchRule token matcher
                = map (\(cat, cons) -> (cat, construct cons matchData)) items
            | otherwise = []

matchRule :: TokenData -> Matcher -> Maybe MatchData
matchRule (TokenData { text }) (ExactMatcher pattern)
    | text == pattern  = Just $ MatchData { token = text }
    | otherwise        = Nothing
