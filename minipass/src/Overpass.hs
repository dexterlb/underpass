{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Overpass where

import           Control.Monad.State.Lazy

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import           Context (VarName)

import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Minipass.Intermediate
import qualified LambdaTypes as T
import           LambdaTypes ((/\))
import TypedLambda (TSLTerm(..), uncurryApplication, substitute)

data Statement
    = OutputSet VarName
    | PerformFilter (HashSet OsmType) [VarName] (HashSet FilterExpr) VarName
    | Comment Text

render :: Statement -> Text
render (Comment x) = "/* " <> x <> " */\n"
render (OutputSet var) = "." <> var <> " out;\n"
render (PerformFilter types inputs filters out)
    = renderUnion (map (renderFilter inputs filters) (HS.toList types)) out

renderUnion :: [Text] -> VarName -> Text
renderUnion items var = "( " <> (Text.concat $ map (<> "; ") items) <> ") -> ." <> var <> ";\n"

renderFilter :: [VarName] -> HashSet FilterExpr -> OsmType -> Text
renderFilter vars filters t = Text.concat $ (renderOsmType t) : (map ("." <>) vars) <> (map renderFilterExpr $ HS.toList filters)

renderOsmType :: OsmType -> Text
renderOsmType OsmNode = "node"
renderOsmType OsmRelation = "rel"
renderOsmType OsmWay = "way"
renderOsmType OsmArea = "area"

renderFilterExpr :: FilterExpr -> Text
renderFilterExpr (KvFilter k v) = "[\"" <> k <> "\" = \"" <> v <> "\"]"
renderFilterExpr (AroundFilter dist var) = "(around." <> var <> ":" <> (Text.pack $ show dist) <> ")"
renderFilterExpr (AreaFilter var) = "(area." <> var <> ")"

data FilterExpr
    = KvFilter Text Text
    | AroundFilter Float VarName
    | AreaFilter VarName
    deriving (Show, Eq, Generic)

instance Hashable FilterExpr

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
translateApp [Constant (T.Basic (Num))    (NumLiteral    n)] = pure $ NumValue n
translateApp term@[Constant _ And, left, right]  = translateFilter ((T.typeOf left) /\ (T.typeOf right)) term
translateApp term@[Constant t@(T.Basic (Set _)) (TypeFilter _)] = translateFilter t term
translateApp term@[Constant (T.Application _ (T.Application _ t)) Kv, _, _] = translateFilter t term
translateApp term@[Constant (T.Application _ t) In, _] = translateFilter t term
translateApp term@[Constant (T.Application _ (T.Application _ t)) Around, _, _] = translateFilter t term
translateApp [Lambda (T.Application t@(T.Basic (Set _)) _) _ m, n] = do
    (SetValue nVar) <- translate n
    translate $ substitute m 0 (Constant t $ FreeVar nVar)
translateApp [Constant _ (FreeVar var)] = pure $ SetValue var
translateApp term = fail $ "I don't know how to translate " <> (show term)

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
walkFilterTree [Constant (T.Application _ (T.Basic (Set _))) In, areaTerm] = do
    (SetValue area) <- translate areaTerm
    pure ([], HS.singleton $ AreaFilter area)
walkFilterTree [Constant (T.Application _ (T.Application _ (T.Basic (Set _)))) Around, distTerm, fromTerm] = do
    (NumValue dist) <- translate distTerm
    (SetValue from) <- translate fromTerm
    pure ([], HS.singleton $ AroundFilter dist from)
walkFilterTree [Constant _ (TypeFilter _)]
    = pure ([], HS.empty)
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
