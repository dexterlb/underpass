{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Overpass where

import           Control.Monad.State.Lazy

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import           Context (VarName)

import Minipass.Intermediate
import qualified LambdaTypes as T
import TypedLambda (TSLTerm(..), uncurryApplication)

data Statement
    = GetKv Text Text VarName
    | PerformAnd VarName VarName VarName
    | OutputSet VarName

render :: Statement -> Text
render (OutputSet var) = "out " <> var <> ";\n"
render (GetKv k v out) = "node[\"" <> k <> "\" = \"" <> v <> "\"] -> ." <> out <> ";\n"
render (PerformAnd left right out) = "node." <> left <> "." <> right <> " -> ." <> out <> ";\n"


data Value
    = StringValue Text
    | NumValue    Float
    | SetValue    VarName

data Translator = Translator
    { lastVarIndex :: Int
    , statements :: [Statement] }

newVar :: State Translator VarName
newVar = do
    state <- get
    let (Translator { lastVarIndex }) = state
    let varIndex = lastVarIndex + 1
    put (state { lastVarIndex = varIndex })
    return $ "x" <> (Text.pack $ show varIndex)

statement :: Statement -> State Translator ()
statement s = do
    state <- get
    let (Translator { statements }) = state
    put (state { statements = s : statements })

translate :: TTerm -> State Translator Value
translate t = translateApp (T.typeOf t) (uncurryApplication t)

translateApp :: (T.ApplicativeType Types) -> [TTerm] -> State Translator Value
translateApp (T.Basic (String)) [Constant _ (StringLiteral s)] = pure $ StringValue s
translateApp (T.Basic (Set _)) [Constant _ Kv, keyTerm, valueTerm] = do
    (StringValue key)   <- translate keyTerm
    (StringValue value) <- translate valueTerm
    result <- newVar
    statement (GetKv key value result)
    return (SetValue result)
translateApp (T.Basic (Set _)) [Constant _ And, leftTerm, rightTerm] = do
    (SetValue left) <- translate leftTerm
    (SetValue right) <- translate rightTerm
    result <- newVar
    statement (PerformAnd left right result)
    return (SetValue result)

translator :: Translator
translator = Translator { lastVarIndex = 0, statements = [] }

tr :: TTerm -> Text
tr t = renderProgram $ runState (translate t) translator

tri :: TTerm -> IO()
tri = TIO.putStrLn . tr

renderProgram :: (Value, Translator) -> Text
renderProgram (SetValue var, Translator { statements })
    = (Text.concat $ map render (extra ++ (reverse statements)))
    where
        extra = [OutputSet var]
