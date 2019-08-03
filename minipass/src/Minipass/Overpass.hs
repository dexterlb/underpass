{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Minipass.Overpass where

import           Control.Monad.State.Lazy
import qualified Control.Monad.Fail as Fail

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

data Statement
    = OutputSet VarName
    | PerformFilters [Filter] VarName
    | Comment Text

data Filter = Filter (HashSet OsmType) [VarName] (HashSet FilterExpr)

render :: Statement -> Text
render (Comment x) = "/* " <> x <> " */\n"
render (OutputSet var) = "." <> var <> " out;\n"
render (PerformFilters filters out)
    = renderUnion (concat $ map renderFilter filters) out

renderUnion :: [Text] -> VarName -> Text
renderUnion items var = "( " <> Text.concat (map (<> "; ") items) <> ") -> ." <> var <> ";\n"

renderFilter :: Filter -> [Text]
renderFilter (Filter types inputs exprs)
    = map (renderFilterCase inputs exprs) (HS.toList types)

renderFilterCase :: [VarName] -> HashSet FilterExpr -> OsmType -> Text
renderFilterCase vars filters t = Text.concat $ renderOsmType t : map ("." <>) vars ++ map renderFilterExpr (HS.toList filters)

renderOsmType :: OsmType -> Text
renderOsmType OsmNode = "node"
renderOsmType OsmRelation = "rel"
renderOsmType OsmWay = "way"
renderOsmType OsmArea = "area"

renderFilterExpr :: FilterExpr -> Text
renderFilterExpr (KvFilter c k v) = "[\"" <> k <> "\" " <> c <> " \"" <> v <> "\"]"
renderFilterExpr (AroundFilter dist var) = "(around." <> var <> ":" <> Text.pack (show dist) <> ")"
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

newVar :: EState Translator VarName
newVar = do
    trans <- get
    let Translator { lastVarIndex } = trans
        varIndex = lastVarIndex + 1
    put (trans { lastVarIndex = varIndex })
    pure $ "x" <> Text.pack (show varIndex)

statement :: Statement -> EState Translator ()
statement s = do
    trans <- get
    let Translator { statements } = trans
    put (trans { statements = s : statements })

expression :: (VarName -> Statement) -> EState Translator VarName
expression f = do
    result <- newVar
    statement $ f result
    pure result

translate :: TTerm -> EState Translator Value
translate t = translateApp (uncurryApplication t)

translateApp :: [TTerm] -> EState Translator Value
translateApp [Constant (T.Basic String) (StringLiteral s)] = pure $ StringValue s
translateApp [Constant (T.Basic Num)    (NumLiteral    n)] = pure $ NumValue n
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
translateApp term@[Constant (T.Application _ (T.Application _ t)) And, _, _]  = translateFilters t term
translateApp term@[Constant (T.Application _ (T.Application _ t))  Or, _, _]  = translateFilters t term
translateApp term@[Constant (T.Application _ (T.Application _ t)) Next, _, _] = translateFilters t term
translateApp term@[Constant (T.Application _ t) Get, _] = translateFilters t term
translateApp [Lambda (T.Application t@(T.Basic (GSet _)) _) _ m, n] = do
    (SetValue nVar) <- translate n
    translate $ substitute m 0 (Application t (Constant (T.Application (T.Basic List) t) Get) (typify emptyContext $ toIntermediate $ listTerm [StringC "setVariable", StringC nVar]))
translateApp term = fail $ "I don't know how to translate " <> show term

translateFilters :: TTypes -> [TTerm] -> EState Translator Value
translateFilters t term = do
    filters <- walkFilterTree t term
    result  <- expression $ PerformFilters filters
    pure $ SetValue result

walkFilterTree :: TTypes -> [TTerm] -> EState Translator [Filter]
walkFilterTree _ [Constant _ Or, left, right] = do
    leftFilters  <- walkFilterTree (T.typeOf  left) $ uncurryApplication left
    rightFilters <- walkFilterTree (T.typeOf right) $ uncurryApplication right
    pure $ leftFilters ++ rightFilters
walkFilterTree t term = do
    let tag = case t of
                (T.Basic (GSet tag')) -> tag'
                other                -> error $ "trying to perform Or on non-set type " <> show other
    (vars, filters) <- walkConjunctiveFilterTree term
    pure $ [Filter (osmTypes tag) vars filters]


walkConjunctiveFilterTree :: [TTerm] -> EState Translator ([VarName], HashSet FilterExpr)
walkConjunctiveFilterTree [Constant _ And, leftTerm, rightTerm] = do
    (sets1, filters1) <- walkConjunctiveFilterTree $ uncurryApplication leftTerm
    (sets2, filters2) <- walkConjunctiveFilterTree $ uncurryApplication rightTerm
    pure (sets1 <> sets2, HS.union filters1 filters2)
walkConjunctiveFilterTree [Constant (T.Application _ _) Get, labelTerm] = do
    (ListValue label)   <- translate labelTerm
    case label of
        [StringC "setVariable", StringC var] -> pure ([var], HS.empty)
        [StringC "tagFilter", ListC [StringC comparator, StringC key, StringC value]]
            -> pure ([], HS.singleton $ KvFilter comparator key value)
        [StringC "all"] -> pure ([], HS.empty)  -- only filter the type, pure all objects
        _   -> fail $ "I don't know how to interpret this label: " <> show label
walkConjunctiveFilterTree [Constant (T.Application _ (T.Application _ _)) Next, labelTerm, inTerm] = do
    (ListValue label)   <- translate labelTerm
    (SetValue  input)   <- translate inTerm
    case label of
        [StringC "in"] -> pure ([], HS.singleton $ AreaFilter input)
        [StringC "around", NumC dist] -> pure ([], HS.singleton $ AroundFilter dist input)
        _   -> fail $ "I don't know how to interpret this label: " <> show label
walkConjunctiveFilterTree terms = do
    (SetValue result) <- translateApp terms
    pure ([result], HS.empty)

translator :: Translator
translator = Translator { lastVarIndex = 0, statements = [] }

translateValue :: TTerm -> Value
translateValue t = fst $ runEState (translate t) translator

tr :: TTerm -> Text
tr t = renderProgram $ runEState (translate t) translator

tri :: TTerm -> IO ()
tri = TIO.putStr . tr

renderProgram :: (Value, Translator) -> Text
renderProgram (value, Translator { statements })
    = Text.concat $ map render $ reverse statements ++ outputValue value
    where
        outputValue (SetValue var)  = [OutputSet var]
        outputValue (NumValue x)    = [Comment $ "Number: " <> Text.pack (show x)]
        outputValue (ListValue x)   = [Comment $ "List: " <> Text.pack (show x)]
        outputValue (StringValue x) = [Comment $ "String: " <> x]

-- here comes a state monad which throws errors. todo: make it more elegant

type EState s = StateT s (EitherError)

newtype EitherError a = EitherError (Either String a) deriving (Functor, Applicative, Monad)

instance Fail.MonadFail (EitherError) where
    fail = EitherError . Left

runEState :: EState s a -> s -> (a, s)
runEState m v = failLeft $ runStateT m v

failLeft :: EitherError a -> a
failLeft (EitherError (Left  s)) = error s
failLeft (EitherError (Right x)) = x


