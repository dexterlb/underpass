{-# LANGUAGE FlexibleInstances #-}

module Ccg.Rules where

import           Data.Text (Text)

import           Utils.Parsing (Parseable, parser)
import           LambdaCalculus.Lambda (LambdaTerm)
import           LambdaCalculus.LambdaTypes (Typed)

type Token = Text

data Rule cat payload = Rule Matcher (cat, PayloadConstructor payload)

data MatchData = MatchData
    { token :: Token
    }

newtype PayloadConstructor payload = PayloadConstructor (MatchData -> payload)

data LambdaPayload t c = LambdaPayload (LambdaTerm t c) MatchData

instance (Typed c t, Parseable t, Parseable c) => Parseable (PayloadConstructor (LambdaPayload t c)) where
    parser = do
        term <- parser
        pure $ PayloadConstructor $ LambdaPayload term

data Matcher
    = ExactMatcher Token
