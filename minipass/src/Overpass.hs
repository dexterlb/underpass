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

import qualified Data.HashSet as HS
import Data.HashSet (HashSet)

import Minipass.Intermediate
import qualified LambdaTypes as T
import TypedLambda (TSLTerm(..), uncurryApplication)

data Statement
    = OutputSet VarName
    | PerformFilter (HashSet OsmType) [VarName] (HashSet FilterExpr) VarName
    | GetInArea (HashSet OsmType) VarName VarName
    | Comment Text

render :: Statement -> Text
render (Comment x) = "/* " <> x <> " */\n"
render (OutputSet var) = "." <> var <> " out;\n"
render (PerformFilter types inputs filters out)
    = renderUnion (map (renderFilter inputs filters) (HS.toList types)) out
render (GetInArea types var out)
    = renderUnion (map (renderInArea var) (HS.toList types)) out

renderUnion :: [Text] -> VarName -> Text
renderUnion items var = "( " <> (Text.concat $ map (<> "; ") items) <> ") -> ." <> var <> ";\n"

renderFilter :: [VarName] -> HashSet FilterExpr -> OsmType -> Text
renderFilter vars filters t = Text.concat $ (renderOsmType t) : (map ("." <>) vars) <> (map (("[" <>) . (<> "]") . renderFilterExpr) $ HS.toList filters)

renderInArea :: VarName -> OsmType -> Text
renderInArea var t = renderOsmType t <> "(area." <> var <> ")"

renderOsmType :: OsmType -> Text
renderOsmType OsmNode = "node"
renderOsmType OsmRelation = "rel"
renderOsmType OsmWay = "way"
renderOsmType OsmArea = "area"

renderFilterExpr :: FilterExpr -> Text
renderFilterExpr (KvFilter k v) = "\"" <> k <> "\" = \"" <> v <> "\""

data Value
    = StringValue Text
    | NumValue    Float
    | SetValue    VarName

data Translator = Translator
    { lastVarIndex :: Int
    , statements :: [Statement] }

newVar :: State Translator VarName
newVar = do
    trans <- get
    let (Translator { lastVarIndex }) = trans
    let varIndex = lastVarIndex + 1
    put (trans { lastVarIndex = varIndex })
    return $ "x" <> (Text.pack $ show varIndex)

statement :: Statement -> State Translator ()
statement s = do
    trans <- get
    let (Translator { statements }) = trans
    put (trans { statements = s : statements })

expression :: (VarName -> Statement) -> State Translator VarName
expression f = do
    result <- newVar
    statement $ f result
    return result

translate :: TTerm -> State Translator Value
translate t = translateApp (uncurryApplication t)

translateApp :: [TTerm] -> State Translator Value
translateApp [Constant (T.Basic (String)) (StringLiteral s)] = pure $ StringValue s
translateApp term@[Constant _ And, left, right]  = translateFilter (T.unify (T.typeOf left) (T.typeOf right)) term
translateApp term@[Constant t@(T.Basic (Set _)) (Filter _)] = translateFilter t term
translateApp term@[Constant (T.Application _ (T.Application _ t)) Kv, _, _] = translateFilter t term
translateApp [Constant (T.Application _ (T.Basic (Set tag))) In, areaTerm] = do
    (SetValue area) <- translate areaTerm
    result <- expression $ GetInArea (osmTypes tag) area
    return (SetValue result)
translateApp term = error $ "I don't know how to translate " <> (show term)

translateFilter :: TTypes -> [TTerm] -> State Translator Value
translateFilter t terms = do
    let (T.Basic (Set tag)) = t
    (vars, filters) <- walkFilterTree terms
    result <- expression $ PerformFilter (osmTypes tag) vars filters
    return $ SetValue result

walkFilterTree :: [TTerm] -> State Translator ([VarName], (HashSet FilterExpr))
walkFilterTree [Constant _ And, leftTerm, rightTerm] = do
    (sets1, filters1) <- walkFilterTree $ uncurryApplication leftTerm
    (sets2, filters2) <- walkFilterTree $ uncurryApplication rightTerm
    return ((sets1 <> sets2), (HS.union filters1 filters2))
walkFilterTree [Constant (T.Application _ (T.Application _ _)) Kv, keyTerm, valueTerm] = do
    (StringValue key)   <- translate keyTerm
    (StringValue value) <- translate valueTerm
    pure ([], HS.singleton $ KvFilter key value)
walkFilterTree [Constant _ (Filter filters)]
    = pure ([], filters)
walkFilterTree terms = do
    (SetValue result) <- translateApp terms
    return ([result], HS.empty)

translator :: Translator
translator = Translator { lastVarIndex = 0, statements = [] }

tr :: TTerm -> Text
tr t = renderProgram $ runState (translate t) translator

tri :: TTerm -> IO()
tri = TIO.putStr . tr

renderProgram :: (Value, Translator) -> Text
renderProgram (value, Translator { statements })
    = (Text.concat $ map render ((reverse statements) ++ (outputValue value)))
    where
        outputValue (SetValue var)  = [OutputSet var]
        outputValue (NumValue x)    = [Comment $ "Number: " <> (Text.pack $ show x)]
        outputValue (StringValue x) = [Comment $ "String: " <> x]
