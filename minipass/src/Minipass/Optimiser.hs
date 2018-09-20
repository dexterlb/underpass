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

import Debug.Trace (trace, traceShow, traceShowId)

optimise :: TTerm -> TTerm
optimise =
    (fixedPoint (removeRestrictions . propagateTypes))

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
