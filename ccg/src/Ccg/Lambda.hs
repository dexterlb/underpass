{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ccg.Lambda where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Utils.Exception (Exception, throw)
import           Data.Dynamic (Typeable)

import           Ccg.Category
import           Ccg.Rules
import           Ccg.Trees (ParseTree(..))

import           Utils.Maths
import           LambdaCalculus.Lambda (LambdaTerm(..))
import qualified LambdaCalculus.Lambda as L
import           LambdaCalculus.LambdaTypes (Typed, typeOf, ApplicativeType(..), apply)
import           LambdaCalculus.TypedLambda (inferTypesOnClosedTerm)
import           Utils.Parsing (Parseable, parser, (<|>), Parser)
import           Utils.Latex

import qualified Utils.Parsing as P

class Compositional a where
    compose :: (Typed c t) => a -> LambdaTerm t c -> LambdaTerm t c -> LambdaTerm t c

treeTerm :: forall t c cat.
            (MSemiLattice (ApplicativeType t), Typed c t, Typed cat t, Combines cat, Compositional (CombineRule cat))
         => ParseTree cat (LambdaTerm t c) -> LambdaTerm t c
treeTerm (Leaf cat term)
    | typeOfCat <!> typeOfTerm = term
    | otherwise = error $
      "term/category inconsistency: " <> show (term, typeOfTerm) <> " !~ " <> show (cat, typeOfCat)
    where
        typeOfCat  = typeOf cat  :: ApplicativeType t
        typeOfTerm = typeOf term :: ApplicativeType t
treeTerm (Vert cat rule left right)
    | typeOfCat <!> typeOfTerm = term
    | otherwise = error $
      "term/category inconsistency at inner node: " <> show (term, typeOfTerm) <> " !~ " <> show (cat, typeOfCat)
    where
        term = compose rule (treeTerm left) (treeTerm right)
        typeOfCat  = typeOf cat  :: ApplicativeType t
        typeOfTerm = typeOf term :: ApplicativeType t

data Template t c = Template Text (Parser (LambdaTerm t c)) (ApplicativeType t)

instantiate :: (Typed c t, Eq c, Eq t) => Template t c -> TokenData -> LambdaTerm t c
instantiate (Template templ parser' typ) (TokenData { tags })
    | typeOf term == typ = term
    | otherwise          = throw $ TemplateTypeMismatch term (typeOf term) typ
    where
        term = inferTypesOnClosedTerm typ $ P.forceParse parser' (foldr sub templ tags)

        sub :: (Text, Text) -> Text -> Text
        sub (tag, value) templ' = Text.replace ("$" <> tag) value templ'

data LambdaPayload t c
    = LambdaPayload   (LambdaTerm t c) TokenData

instance (Eq c, Eq t, Typed c t) => FromMatch (LambdaPayload t c) where
    type Constructor (LambdaPayload t c) = LambdaConstructor t c

    construct (SimpleLambdaConstructor t) dat = LambdaPayload t dat
    construct (TemplateLambdaConstructor t templ) dat
        = LambdaPayload (L.Application t (instantiate templ dat)) dat

data LambdaConstructor t c
    = SimpleLambdaConstructor   (LambdaTerm t c)
    | TemplateLambdaConstructor (LambdaTerm t c) (Template t c)

instance (Typed c t, Parseable t, Parseable c) => Parseable (LambdaConstructor t c) where
    parser = (P.operator "@") *> (templateParser <|> simpleParser)
        where
            simpleParser = P.try $ do
                term <- parser
                pure $ SimpleLambdaConstructor term

            templateParser = P.try $ do
                term     <- parser
                template <- P.quotedString '`'
                _        <- P.operator ":"
                typ      <- parser

                pure $ TemplateLambdaConstructor term (Template template parser typ)

instance (Typed c t) => Typed (LambdaConstructor t c) t where
    typeOf (SimpleLambdaConstructor   term)                    = typeOf term
    typeOf (TemplateLambdaConstructor term (Template _ _ arg)) = apply (typeOf term) arg

instance (Show c, Show t) => Show (LambdaPayload t c) where
    show (LambdaPayload   t d)      = show d <> " @ " <> show t

instance (Show t, Show c) => Show (Template t c) where
    show (Template term _ typ) = "`" <> show term <> "`: " <> show typ

instance (Show c, Show t) => Show (LambdaConstructor t c) where
    show (SimpleLambdaConstructor   t)      = " @ " <> show t
    show (TemplateLambdaConstructor t temp) = " @ " <> show t <> " " <> show temp

instance (Show t, Show c) => Latexable (LambdaPayload t c) where
    latex (LambdaPayload   _term tokendata) = latex tokendata

data TemplateException t c
    = TemplateTypeMismatch (LambdaTerm t c) (ApplicativeType t) (ApplicativeType t)
instance (Show t, Show c, Typeable t, Typeable c) => Exception (TemplateException t c)
deriving instance (Show t, Show c) => Show (TemplateException t c)
