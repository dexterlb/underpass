{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Minipass.Overpass where

import           Control.Monad.State.Lazy

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import           LambdaCalculus.Context (VarName, emptyContext)
import           LambdaCalculus.TypedLambda (TSLTerm(..), uncurryApplication, substitute, typify)
import qualified LambdaCalculus.LambdaTypes as T

import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Minipass.Language.Intermediate
import Minipass.Language.Constants
import Minipass.Language.Language (ListC(..), listTerm)

import           Utils.Maths ((/\))

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
renderFilterExpr (KvFilter c k v) = "[\"" <> k <> "\" " <> c <> " \"" <> v <> "\"]"
renderFilterExpr (AroundFilter dist var) = "(around." <> var <> ":" <> (Text.pack $ show dist) <> ")"
renderFilterExpr (AreaFilter var) = "(area." <> var <> ")"

data FilterExpr
    = KvFilter Text Text Text
    | AroundFilter Float VarName
    | AreaFilter VarName
    deriving (Show, Eq, Generic)

instance Hashable FilterExpr

data Value
    = StringValue Text
    | NumValue    Float
    | SetValue    VarName
    | ListValue   [ListC]

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
translateApp [Constant _ Empty] = pure $ ListValue []
translateApp [Constant _ ConsString, xTerm, xsTerm] = do
    (StringValue x) <- translate xTerm
    (ListValue  xs) <- translate xsTerm
    pure $ ListValue (StringC x : xs)
translateApp [Constant _ ConsNum, xTerm, xsTerm] = do
    (NumValue   x) <- translate xTerm
    (ListValue xs) <- translate xsTerm
    pure $ ListValue (NumC x : xs)
translateApp [Constant _ ConsList, xTerm, xsTerm] = do
    (ListValue  x) <- translate xTerm
    (ListValue xs) <- translate xsTerm
    pure $ ListValue (ListC x : xs)
translateApp term@[Constant _ And, left, right]  = translateFilter ((T.typeOf left) /\ (T.typeOf right)) term
translateApp term@[Constant (T.Application _ (T.Application _ t)) Next, _, _] = translateFilter t term
translateApp term@[Constant (T.Application _ t) Get, _] = translateFilter t term
translateApp [Lambda (T.Application t@(T.Basic (Set _)) _) _ m, n] = do
    (SetValue nVar) <- translate n
    translate $ substitute m 0 (Application t (Constant (T.Application (T.Basic List) t) Get) (typify emptyContext $ toIntermediate $ listTerm [StringC "setVariable", StringC nVar]))
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
walkFilterTree [Constant (T.Application _ _) Get, labelTerm] = do
    (ListValue label)   <- translate labelTerm
    case label of
        [StringC "setVariable", StringC var] -> pure ([var], HS.empty)
        [StringC "tagFilter", ListC [StringC comparator, StringC key, StringC value]]
            -> pure ([], HS.singleton $ KvFilter comparator key value)
        [StringC "all"] -> pure ([], HS.empty)  -- only filter the type, return all objects
        _   -> fail $ "I don't know how to interpret this label: " <> show label
walkFilterTree [Constant (T.Application _ (T.Application _ _)) Next, labelTerm, inTerm] = do
    (ListValue label)   <- translate labelTerm
    (SetValue  input)   <- translate inTerm
    case label of
        [StringC "in"] -> pure ([], HS.singleton $ AreaFilter input)
        [StringC "around", NumC dist] -> pure ([], HS.singleton $ AroundFilter dist input)
        _   -> fail $ "I don't know how to interpret this label: " <> show label
walkFilterTree terms = do
    (SetValue result) <- translateApp terms
    return ([result], HS.empty)

translator :: Translator
translator = Translator { lastVarIndex = 0, statements = [] }

translateValue :: TTerm -> Value
translateValue t = fst $ runState (translate t) translator

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
        outputValue (ListValue x)   = [Comment $ "List: " <> (Text.pack $ show x)]
        outputValue (StringValue x) = [Comment $ "String: " <> x]
