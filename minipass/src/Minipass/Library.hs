{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Minipass.Library where

import Lambda
import Minipass.Language
import Context
import Parsing (Parser)

library :: [(VarName, Term)]
library =
    [ ("name",    pts "lambda n : String { get (consString 'tagFilter' (consList (consString '==' (consString 'name' (consString n empty))) empty)) }")
    , ("amenity", pts "lambda n : String { get (consString 'tagFilter' (consList (consString '==' (consString 'amenity' (consString n empty))) empty)) }")
    , ("within",  pts "lambda dist : Num { next (consString 'around'   (consNum dist empty)) }")
    , ("in",      pts "next (consString 'in' empty)")
    ]

parseWithLibrary :: Parser Term
parseWithLibrary = parseWrap library
