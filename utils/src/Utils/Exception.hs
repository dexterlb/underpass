module Utils.Exception
    ( D.Typeable
    , E.Exception
    , throw
    ) where

import qualified Control.Exception as E
import qualified Data.Dynamic as D

throw :: E.Exception e => e -> a
-- throw = E.throw
throw = debugThrow

debugThrow :: E.Exception e => e -> a
debugThrow e = error $ "exception: " <> show e
