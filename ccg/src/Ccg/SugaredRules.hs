{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Ccg.SugaredRules where

import           Data.Text (Text)


import           Utils.Parsing (Parseable(..), (<|>))
import qualified Utils.Parsing as P
import           Ccg.Rules (Matcher(..))
import           Ccg.Lambda (Template(..))
import           Ccg.Modal (ModalCategory)
import           LambdaCalculus.LambdaTypes (UnresolvedType, Ref, Typed(..))
import           LambdaCalculus.Lambda (LambdaTerm(..))
import           Utils.Maths

data NamedMatcher       = NamedMatcher Text Matcher
data NamedTemplate t c  = NamedTemplate Text (Template t c)

data SugaredMatcher
    = Single NamedMatcher
    | Concat NamedMatcher Text SugaredMatcher

data SugaredRule t c = SugaredRule SugaredMatcher [(ModalCategory (UnresolvedType t), LambdaTerm (Ref t) (Ref c), [NamedTemplate (Ref t) (Ref c)])]

instance Parseable NamedMatcher where
    parser = do
        name    <- (P.try $ P.identifier <* P.operator "#") <|> pure "_"
        matcher <- parser
        pure $ NamedMatcher name matcher

instance (Typed c t, Parseable t, Parseable c) => Parseable (NamedTemplate t c) where
    parser = do
        name     <- (P.try $ P.identifier <* P.operator "#") <|> pure "_"
        templateString <- P.quotedString '`'
        _              <- P.operator ":"
        typ            <- P.parser
        pure $ NamedTemplate name $ Template templateString parser typ

instance Parseable SugaredMatcher where
    parser = concatParser <|> singleParser
        where
            singleParser = Single <$> parser
            concatParser = P.try $ do
                matcher <- parser
                cname   <- P.operator "<" *> (P.identifier <|> pure "Foo") <* P.operator ">"
                next    <- parser
                pure $ Concat matcher cname next

instance (Eq t, Typed (Ref c) (Ref t), PartialOrd t, Parseable t, Parseable c) => Parseable (SugaredRule t c) where
    parser = do
        matcher <- parser
        _       <- P.operator "."
        targets <- P.separated "," $ do
            cat         <- parser
            term        <- parser
            templates   <- P.many parser
            pure (cat, term, templates)
        _       <- P.operator "."
        pure $ SugaredRule matcher targets
