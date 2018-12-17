{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Minipass.Language.Library where

import Minipass.Lambda
import Minipass.Language.Language
import Minipass.Context

import Utils.Parsing (Parser)

library :: [(VarName, Term)]
library =
    [ ("name",    pts "lambda n : String { get (consString 'tagFilter' (consList (consString '==' (consString 'name' (consString n empty))) empty)) }")
    , ("amenity", pts "lambda n : String { get (consString 'tagFilter' (consList (consString '==' (consString 'amenity' (consString n empty))) empty)) }")
    , ("within",  pts "lambda dist : Num { next (consString 'around'   (consNum dist empty)) }")
    , ("in",      pts "next (consString 'in' empty)")
    , ("nodes",   pts "get (consString 'all' (consString 'nodes' empty))")
    , ("ways",   pts "get (consString 'all' (consString 'ways' empty))")
    , ("relations",   pts "get (consString 'all' (consString 'relations' empty))")
    , ("areas",   pts "get (consString 'all' (consString 'areas' empty))")
    ]

parseWithLibrary :: Parser Term
parseWithLibrary = parseWrap library
