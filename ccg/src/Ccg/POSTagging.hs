{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ccg.POSTagging where

import           System.IO.Unsafe (unsafePerformIO)
import           Data.List.NonEmpty (toList)

import qualified NLP.Types.Tree as N
import qualified NLP.Types.Tags as N
import qualified NLP.Types as N
import qualified NLP.POS as N
import qualified NLP.Corpora.Conll as NC

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified WordNet.DB as WN

import           Ccg.Rules

type MetaTagger t = Text -> t -> [Tag]

simpleEnglishPosTaggingLexer :: IO Lexer
simpleEnglishPosTaggingLexer = do
    mt <- englishMetaTagger
    pt <- N.defaultTagger
    pure $ posTaggingLexer mt pt


englishMetaTagger :: IO (MetaTagger NC.Tag)
englishMetaTagger = do
    _     <- WN.ensureInit
    pure $ mt

    where
        mt w posTag
            = map (\x -> ("lemma", x)) $ toList $ unsafePerformIO $ WN.morph1 w (toWN posTag)
            where
                toWN :: NC.Tag -> WN.POS
                toWN NC.JJ         = WN.Adj
                toWN NC.JJR        = WN.Adj
                toWN NC.JJS        = WN.Adj
                toWN NC.NN         = WN.Noun
                toWN NC.NNS        = WN.Noun
                toWN NC.NNP        = WN.Noun
                toWN NC.NNPS       = WN.Noun
                toWN NC.RB         = WN.Adv
                toWN NC.RBR        = WN.Adv
                toWN NC.RBS        = WN.Adv
                toWN NC.VB         = WN.Verb
                toWN NC.VBD        = WN.Verb
                toWN NC.VBG        = WN.Verb
                toWN NC.VBN        = WN.Verb
                toWN NC.VBP        = WN.Verb
                toWN NC.VBZ        = WN.Verb
                toWN _             = WN.Any

posTaggingLexer :: N.Tag t => MetaTagger t -> N.POSTagger t -> Lexer
posTaggingLexer mt tagger text = mergeSentences mt $ N.tag tagger text

mergeSentences :: N.Tag t => MetaTagger t -> [N.TaggedSentence t] -> [TokenData]
mergeSentences mt = concat . (map ((sentBegin ++) . (++ sentEnd) . (processSentence mt)))

processSentence :: N.Tag t => MetaTagger t -> N.TaggedSentence t -> [TokenData]
processSentence mt (N.TaggedSent tokens) = map (processToken mt) tokens

processToken :: N.Tag t => MetaTagger t -> N.POS t -> TokenData
processToken mt (N.POS { N.posTag, N.posToken = (N.Token posToken) }) = TokenData
    { text = posToken
    , tags =
        [ ("pos", replaceTag $ Text.toLower $ N.fromTag posTag)
        , ("raw", posToken)
        ] ++ (mt posToken posTag)
    }

sentBegin :: [TokenData]
sentBegin = pure $ TokenData
    { text = ""
    , tags = [("mark", "begin")]
    }

sentEnd :: [TokenData]
sentEnd = pure $ TokenData
    { text = ""
    , tags = [("mark", "end")]
    }

replaceTag :: Text -> Text
replaceTag "," = "comma"
replaceTag x   = x
