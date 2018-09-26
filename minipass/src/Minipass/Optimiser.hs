{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Minipass.Optimiser where

import Minipass.Intermediate
import qualified LambdaTypes as T
import TypedLambda
import Maths

import qualified Data.HashSet as HS


optimise :: TTerm -> TTerm
optimise
    = evaluateFilters
    . evaluateArithmetic
    . (fixedPoint (removeRestrictions . propagateTypes))

propagateTypes :: TTerm -> TTerm
propagateTypes = (updateTypes updater) . fixTypes
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
        updater term = T.typeOf term

removeRestrictions :: TTerm -> TTerm
removeRestrictions = id

evaluateArithmetic :: TTerm -> TTerm
evaluateArithmetic = id

evaluateFilters :: TTerm -> TTerm
evaluateFilters = transformApplications f
    where
        f [Constant (T.Application _ (T.Application _ t)) Kv, Constant _ (StringLiteral k), Constant _ (StringLiteral v)]
            = Just $ Constant t (Filter $ HS.singleton $ KvFilter k v)

        -- the following can be inferred from the type
        f [Constant t Nodes]
            = Just $ Constant t (Filter $ HS.empty)
        f [Constant t Ways]
            = Just $ Constant t (Filter $ HS.empty)
        f [Constant t Relations]
            = Just $ Constant t (Filter $ HS.empty)
        f [Constant t Areas]
            = Just $ Constant t (Filter $ HS.empty)

        f _ = Nothing

