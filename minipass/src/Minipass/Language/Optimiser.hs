{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Minipass.Language.Optimiser where

import Minipass.Language.Intermediate
import qualified Minipass.LambdaTypes as T
import           Minipass.LambdaTypes (typeOf)
import Minipass.TypedLambda
import Utils.Maths
import Minipass.Language.Constants
import Minipass.Language.Language (ListC(..), listTerm)
import Minipass.Overpass (Value(ListValue), translateValue)
import Minipass.Context

optimise :: TTerm -> TTerm
optimise
    = fixedPoint (
        evaluateArithmetic
        . generaliseUniversals
        . (fixedPoint (propagateTypes . fixTypes))
        . (fixedPoint (betaReduce reducible))
        . (fixedPoint fixTypes)
    )

propagateTypes :: TTerm -> TTerm
propagateTypes = updateTypes updater
    where
        updater (Constant
                 (T.Application (T.Basic (Set a)) (T.Application (T.Basic (Set b)) (T.Basic (Set c))))
                 And)
                 = T.Application (T.Basic (Set n)) (T.Application (T.Basic (Set n)) (T.Basic (Set n)))
                where
                    n = intersectSetTags c $ intersectSetTags a b
        updater (Constant
                 (T.Application (T.Basic (Set a)) (T.Application (T.Basic (Set b)) (T.Basic (Set c))))
                 Or)
                 = T.Application (T.Basic (Set na)) (T.Application (T.Basic (Set nb)) (T.Basic (Set nc)))
                where
                    na = intersectSetTags nc a
                    nb = intersectSetTags nc b
                    nc = intersectSetTags c $ uniteSetTags a b
        updater (Application t (Constant _ Get) term)
            | (ListValue l) <- translateValue term = refineGetType l t
            | otherwise                                = t
        updater (Application t (Constant _ Next) term)
            | (ListValue l) <- translateValue term = refineNextType l t
            | otherwise                                = t
        updater term = T.typeOf term

refineGetType :: [ListC] -> TTypes -> TTypes
refineGetType [StringC "tagFilter", ListC [StringC _, StringC "amenity", _]] t
    = t /\ (T.Basic $ osmSet [OsmNode])
refineGetType [StringC "all", StringC "nodes"] t
    = t /\ (T.Basic $ osmSet [OsmNode])
refineGetType [StringC "all", StringC "ways"] t
    = t /\ (T.Basic $ osmSet [OsmWay])
refineGetType [StringC "all", StringC "relations"] t
    = t /\ (T.Basic $ osmSet [OsmRelation])
refineGetType [StringC "all", StringC "areas"] t
    = t /\ (T.Basic $ osmSet [OsmArea])
refineGetType _ t = t

refineNextType :: [ListC] -> TTypes -> TTypes
refineNextType [StringC "in"] t
    = t /\ (T.Application (T.Basic $ osmSet [OsmArea]) (T.Basic $ osmSet [OsmNode, OsmRelation, OsmWay]))
refineNextType _ t = t

generaliseUniversals :: TTerm -> TTerm
generaliseUniversals = transform f
    where
        f (Application t c@(Constant _ Get) term)
            | (ListValue [StringC "all", StringC     "nodes"]) <- list = general
            | (ListValue [StringC "all", StringC      "ways"]) <- list = general
            | (ListValue [StringC "all", StringC "relations"]) <- list = general
            | (ListValue [StringC "all", StringC     "areas"]) <- list = general
            | otherwise = Nothing
            where
                list = translateValue term
                general = pure (Application t c $ typify emptyContext $ toIntermediate $ listTerm [StringC "all"])
        f _ = Nothing

evaluateArithmetic :: TTerm -> TTerm
evaluateArithmetic = id

reducible :: TTerm -> Bool
reducible x = (not $ isSet $ typeOf x) || (trivial x)

isSet :: T.ApplicativeType Types -> Bool
isSet (T.Basic (Set _)) = True
isSet _                 = False

trivial :: TTerm -> Bool
trivial (Application _
            (Constant _ Get)
            (Application _
                (Application _
                    (Constant _ ConsString)
                    (Constant _ (StringLiteral "all"))
                )
                (Constant _ Empty)
            )
        )
            = True
trivial x
    | (T.Basic Num)     <- typeOf x = True
    | (T.Basic String)  <- typeOf x = True
    | (T.Basic List)    <- typeOf x = True
    | otherwise         = False
