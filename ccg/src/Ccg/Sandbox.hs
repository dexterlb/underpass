{-# LANGUAGE OverloadedStrings #-}

module Ccg.Sandbox where

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Text (Text)

import           Ccg.Category
import           Ccg.Trees
import           Ccg.Cyk
import           Ccg.Modal
import           Ccg.Latex

simpleWord :: Vector [(ModalCategory, Text)]
simpleWord = V.fromList
    [ [(sc "A", "a"), (sc "S" </> sc "C" </> sc "B", "a")]
    , [(sc "S" </> sc "C" <\> sc "A", "b"), (sc "B", "b"), (sc "S" </> vc "p" <\> vc "p", "b")]
    , [(sc "C", "c"), (sc "A", "c")]
    ]

simpleCyk :: [ParseTree ModalCategory Text]
simpleCyk = enumTrees $ cyk simpleWord $ Simple $ NonTerm "S"

latexCyk :: IO ()
latexCyk = latexPreview simpleCyk
