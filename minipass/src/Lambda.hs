{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Lambda where

import qualified LambdaTypes as T
import LambdaTypes (Typed, typ)

import Parsing (Parser, Parseable, parser, (<|>))
import qualified Parsing as P

import Data.Text (Text)
import qualified Data.Text as Text

import Data.List (elemIndex)
import Control.Monad (fail, foldM)

type Index = Int
type VarName = Text
newtype VarContext t = VarContext [(VarName, T.ApplicativeType t)]

push :: (VarName, T.ApplicativeType t) -> VarContext t -> VarContext t
push x (VarContext c) = VarContext $ x : c

at :: Index -> VarContext t -> Maybe (VarName, T.ApplicativeType t)
at i (VarContext c)
    | length c > i = Just $ c !! i
    | otherwise = Nothing

get :: VarName -> VarContext t -> Maybe (Index, T.ApplicativeType t)
get name (VarContext c) = (\i -> (i, snd $ c !! i)) <$> (elemIndex name (map fst c))

emptyContext :: VarContext t
emptyContext = VarContext []

data LambdaTerm t c where
    Constant    :: Typed c t => c              -> LambdaTerm t c
    Application :: Typed c t => LambdaTerm t c -> LambdaTerm t c      -> LambdaTerm t c
    Lambda      :: Typed c t => VarName        -> T.ApplicativeType t -> LambdaTerm t c -> LambdaTerm t c
    Variable    :: Typed c t => Index          -> LambdaTerm t c

instance (Show t, Show c) => Show (LambdaTerm t c) where
    show = showTerm emptyContext

showTerm :: (Show t, Show c) => VarContext t -> LambdaTerm t c -> String
showTerm _ (Constant c) = show c
showTerm context (Application a b) = "(" ++ showTerm context a ++ " " ++ showTerm context b ++ ")"
showTerm context (Lambda x t a) = "λ " ++ (Text.unpack x) ++ ": " ++ show t ++ " { " ++ showTerm (push (x, t) context) a ++ " }"
showTerm context (Variable i)
    | Just (x, _) <- at i context = Text.unpack x
    | otherwise = "<var " ++ (show i) ++ ">"


instance (Typed c t, Eq t) => Typed (LambdaTerm t c) t where
    typ = typeOfTerm emptyContext

typeOfTerm :: (Typed c t, Eq t) => VarContext t -> LambdaTerm t c -> T.ApplicativeType t
typeOfTerm _ (Constant c) = typ c
typeOfTerm context (Application a b)
    | (T.Application p q) <- typeOfTerm context a, p == typeOfTerm context b = q
    | otherwise = T.TypeError
typeOfTerm context (Lambda x t a) = T.Application t (typeOfTerm (push (x, t) context) a)
typeOfTerm context (Variable i)
    | Just (_, t) <- at i context = t
    | otherwise = T.TypeError

instance (Parseable t, Parseable c, Typed c t) => Parseable (LambdaTerm t c) where
    parser = fst <$> parseTerm emptyContext

parseTerm :: (Parseable t, Parseable c, Typed c t) => VarContext t -> Parser (LambdaTerm t c, T.ApplicativeType t)
parseTerm context
    =   parseApplication context

parseNonApplication :: (Parseable t, Parseable c, Typed c t) => VarContext t -> Parser (LambdaTerm t c, T.ApplicativeType t)
parseNonApplication context
    =   parseConstant
    <|> parseLambda context
    <|> parseVariable context

parseConstant :: (Parseable t, Parseable c, Typed c t) => Parser (LambdaTerm t c, T.ApplicativeType t)
parseConstant = do
    const <- Constant <$> parser
    return (const, typ const)

parseLambda :: (Parseable t, Parseable c, Typed c t) => VarContext t -> Parser (LambdaTerm t c, T.ApplicativeType t)
parseLambda context = do
    P.word "lambda" <|> P.operator "\\" <|> P.operator "λ"
    (var, varType) <- parseVariableDeclaration
    P.operator "{"
    (term, termType) <- parseTerm (push (var, varType) context)
    P.operator "}"
    return (Lambda var varType term, T.Application varType termType)

parseApplication :: (Parseable t, Parseable c, Typed c t) => VarContext t -> Parser (LambdaTerm t c, T.ApplicativeType t)
parseApplication context = check =<< (((foldl1 makeApplication) . (map Just))
    <$> (P.some $ parseNonApplication context))
    where
        check Nothing = fail "type error when trying to parse application"
        check (Just x) = return x

        makeApplication (Just (_, (T.Application _ T.TypeError))) _ = Nothing
        makeApplication (Just (x, (T.Application a b))) (Just (y, c))
            | c == a = Just (Application x y, b)
            | otherwise = Nothing
        makeApplication _ _ = Nothing

parseVariableDeclaration :: (Parseable t) => Parser (VarName, t)
parseVariableDeclaration = do
    var <- parseVariableName
    P.operator ":"
    varType <- parser
    return (var, varType)

parseVariable :: (Parseable t, Parseable c, Typed c t) => VarContext t -> Parser (LambdaTerm t c, T.ApplicativeType t)
parseVariable context = P.try $ do
    name <- parseVariableName
    case get name context of
        Just (i, t) -> return (Variable i, t)
        Nothing     -> fail "variable or identifier does not exist"

parseVariableName :: Parser VarName
parseVariableName = P.identifier