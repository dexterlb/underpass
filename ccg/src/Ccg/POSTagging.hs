{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ccg.POSTagging where

import qualified NLP.Types.Tree as N
import qualified NLP.Types.Tags as N
import qualified NLP.Types as N
import qualified NLP.POS as N

import qualified Data.HashSet as HS
import qualified Data.Text as Text

import           Ccg.Rules

simpleEnglishPosTaggingLexer :: IO Lexer
simpleEnglishPosTaggingLexer = posTaggingLexer <$> N.defaultTagger

posTaggingLexer :: N.Tag t => N.POSTagger t -> Lexer
posTaggingLexer tagger text = mergeSentences $ N.tag tagger text

mergeSentences :: N.Tag t => [N.TaggedSentence t] -> [TokenData]
mergeSentences = concat . (map ((sentBegin ++) . (++ sentEnd) . processSentence))

processSentence :: N.Tag t => N.TaggedSentence t -> [TokenData]
processSentence (N.TaggedSent tokens) = map processToken tokens

processToken :: N.Tag t => N.POS t -> TokenData
processToken (N.POS { N.posTag, N.posToken = (N.Token posToken) }) = TokenData
    { text = posToken
    , tags = HS.singleton $ Text.toLower $ N.fromTag posTag
    }

sentBegin :: [TokenData]
sentBegin = pure $ TokenData
    { text = ""
    , tags = HS.singleton "begin"
    }

sentEnd :: [TokenData]
sentEnd = pure $ TokenData
    { text = ""
    , tags = HS.singleton "end"
    }
