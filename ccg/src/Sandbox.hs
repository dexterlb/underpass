{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Sandbox where

import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Category
import           Trees
import           Cyk
import           Modal

simpleWord :: Vector [(ModalCategory, String)]
simpleWord = V.fromList
    [ [(sc "A", "a"), (sc "S" </> sc "C" </> sc "B", "a")]
    , [(sc "S" </> sc "C" <\> sc "A", "b" ), (sc "B", "b")]
    , [(sc "C", "c")]
    ]

simpleCyk :: [ParseTree ModalCategory String]
simpleCyk = enumTrees $ cyk simpleWord $ Simple $ NonTerm "S"
