{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ccg.SugaredRules where

import           Data.Text (Text, pack)
import           System.FilePath (takeBaseName)


import           Utils.Parsing (Parseable(..), (<|>))
import qualified Utils.Parsing as P
import           Ccg.Rules (Matcher(..), Rule(..))
import           Ccg.Lambda (Template(..), LambdaConstructor(..))
import           Ccg.Modal (ModalCategory, NonTerm(NonTerm), Slash(..), Modality(..))
import           Ccg.LambdaRules   (UnresolvedLambdaRule)
import           Ccg.Category (Category(..))
import           LambdaCalculus.LambdaTypes (UnresolvedType, Ref(..), Typed(..), ApplicativeType(..))
import           LambdaCalculus.Lambda (LambdaTerm(Lambda, Cast, Variable))
import           LambdaCalculus.UserTypeSystem (SubtypeAssertion(..))
import           Utils.Maths

data TemplatedMatcher t c = TemplatedMatcher Matcher (Maybe (Template (Ref t) (Ref c)))
deriving instance (Show t, Show c) => Show (TemplatedMatcher t c)

data SugaredMatcher t c
    = Single (TemplatedMatcher t c)
    | Concat (TemplatedMatcher t c) Text P.SourcePos (SugaredMatcher t c)

deriving instance (Show t, Show c) => Show (SugaredMatcher t c)

data SugaredRule t c
    = SugaredRule (SugaredMatcher t c) [(ModalCategory (UnresolvedType t), LambdaTerm (Ref t) (Ref c))]

deriving instance (Show t, Show c) => Show (SugaredRule t c)

desugar :: (Typed c t) => SugaredRule t c -> ([UnresolvedLambdaRule t c], [SubtypeAssertion t])
desugar (SugaredRule m targets) = concat2 $ map (uncurry $ desugarSingle m) targets

desugarSingle :: (Typed c t)
              => SugaredMatcher t c
              -> ModalCategory (UnresolvedType t)
              -> LambdaTerm (Ref t) (Ref c)
              -> ([UnresolvedLambdaRule t c], [SubtypeAssertion t])
desugarSingle ms cat term
    | allTemplates ms = (primRuleAllTempl:secRules, secAssertions)
    | noTemplates  ms = ( primRuleNoTempl:secRules, secAssertions)
    | otherwise = error "when matching more than one token at once, either use templates on all tokens or on none."
    where
        primRuleAllTempl = Rule m primCat (TemplateLambdaConstructor term templ)
        primRuleNoTempl  = Rule m primCat (SimpleLambdaConstructor termNoTempl)
        termNoTempl = foldr (\typ term' -> Lambda "_" typ term') term $ map (Basic . fst) t
        primCat = foldr (flip $ Complex (RightSlash Star)) cat $ map (Simple . NonTerm . Basic . fst) t
        (secRules, secAssertions) = concat2 $ map desugarSecondary t
        (Just templ) = maybeTempl   -- lazy evaluation ftw
        (TemplatedMatcher m maybeTempl) = sugarHead ms
        t = sugarTail ms

desugarSecondary :: forall t c. Typed c t => (Ref t, TemplatedMatcher t c) -> ([UnresolvedLambdaRule t c], [SubtypeAssertion t])
desugarSecondary (dummyType@(UnresolvedName _ name), (TemplatedMatcher m (Just templ@(Template _ _ t))))
    = ( [Rule m (Simple $ NonTerm $ Basic $ dummyType)
                (TemplateLambdaConstructor (Lambda "_" t $ Cast (Basic dummyType) $ Variable 0)
                                           templ)]
      , [SubtypeAssertion name $ t] )
desugarSecondary (dummyType@(UnresolvedName _ name), (TemplatedMatcher m Nothing))
    = ( [Rule m (Simple $ NonTerm $ Basic $ dummyType)
                (SimpleLambdaConstructor (Cast (Basic dummyType) $ Lambda "_" Wildcard $ Variable 0)
                                           )]
      , [SubtypeAssertion name $ Application Wildcard Wildcard] )
desugarSecondary (BasicRef _, _) = error "basic type passed to desugar concat"

allTemplates :: SugaredMatcher t c -> Bool
allTemplates (Single (TemplatedMatcher _ (Just _))) = True
allTemplates (Concat (TemplatedMatcher _ (Just _)) _ _ rest) = allTemplates rest
allTemplates _ = False

noTemplates :: SugaredMatcher t c -> Bool
noTemplates (Single (TemplatedMatcher _ Nothing)) = True
noTemplates (Concat (TemplatedMatcher _ Nothing) _ _ rest) = noTemplates rest
noTemplates _ = False

concat2 :: [([a], [b])] -> ([a], [b])
concat2 l = (concatMap fst l, concatMap snd l)

sugarHead :: SugaredMatcher t c -> TemplatedMatcher t c
sugarHead (Single x) = x
sugarHead (Concat x _ _ _) = x

sugarTail :: SugaredMatcher t c -> [(Ref t, TemplatedMatcher t c)]
sugarTail (Single _) = []
sugarTail (Concat _ n p t) = (UnresolvedName p n, sugarHead t):(sugarTail t)

instance (Typed c t, Parseable c, Parseable t) => Parseable (TemplatedMatcher t c) where
    parser = do
        matcher <- parser
        templ   <- (P.try $ Just <$> do
            templateString <- P.quotedString '`'
            _              <- P.operator ":"
            typ            <- P.parser
            pure $ Template templateString parser typ
            ) <|> (pure Nothing)
        pure $ TemplatedMatcher matcher templ

instance (Typed c t, Parseable c, Parseable t) => Parseable (SugaredMatcher t c) where
    parser = concatParser <|> singleParser
        where
            singleParser = Single <$> parser
            concatParser = P.try $ do
                matcher <- parser
                pos     <- P.getSourcePos
                cname   <- P.operator "<" *> (P.identifier <|> pure (genName pos)) <* P.operator ">"
                next    <- parser
                pure $ Concat matcher cname pos next

            genName (P.SourcePos { P.sourceName, P.sourceLine, P.sourceColumn })
                = pack $
                       "Phrase:"
                    <> takeBaseName sourceName
                    <> ":" <> show (P.unPos sourceLine) <> ":" <> show (P.unPos sourceColumn)

instance (Eq t, Typed c t, PartialOrd t, Parseable t, Parseable c) => Parseable (SugaredRule t c) where
    parser = do
        matcher <- parser
        _       <- P.operator ":"
        targets <- P.separated "," $ do
            cat         <- parser
            _           <- P.operator "@"
            term        <- parser
            pure (cat, term)
        _       <- P.operator "."
        pure $ SugaredRule matcher targets
