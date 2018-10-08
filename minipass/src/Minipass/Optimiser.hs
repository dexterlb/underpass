{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Minipass.Optimiser where

import Minipass.Intermediate
import qualified LambdaTypes as T
import           LambdaTypes ((/\))
import TypedLambda
import Maths
import Minipass.Constants
import Minipass.Language (ListC(..))
import Overpass (Value(ListValue), translateValue)


optimise :: TTerm -> TTerm
optimise
    = evaluateArithmetic
    . (fixedPoint (removeRestrictions . propagateTypes . fixTypes))
    . (fixedPoint $ betaReduce (not . isSet))
    . (fixedPoint fixTypes)

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
        updater (Application t (Constant _ Get) listTerm)
            | (ListValue l) <- translateValue listTerm = refineGetType l t
            | otherwise                                = t
        updater (Application t (Constant _ Next) listTerm)
            | (ListValue l) <- translateValue listTerm = refineNextType l t
            | otherwise                                = t
        updater term = T.typeOf term

refineGetType :: [ListC] -> TTypes -> TTypes
refineGetType [StringC "tagFilter", ListC [StringC _, StringC "amenity", _]] t
    = t /\ (T.Basic $ osmSet [OsmNode])
refineGetType _ t = t

refineNextType :: [ListC] -> TTypes -> TTypes
refineNextType [StringC "in"] t
    = t /\ (T.Application (T.Basic $ osmSet [OsmArea]) (T.Basic $ osmSet [OsmNode, OsmRelation, OsmWay]))
refineNextType _ t = t

removeRestrictions :: TTerm -> TTerm
removeRestrictions = id

evaluateArithmetic :: TTerm -> TTerm
evaluateArithmetic = id

isSet :: T.ApplicativeType Types -> Bool
isSet (T.Basic (Set _)) = True
isSet _                 = False
