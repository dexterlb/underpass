{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils.Resolver where

-- import qualified Data.HashMap.Lazy as HM
import           Data.HashMap.Lazy (HashMap)
import           Data.HashSet (HashSet)
import           Data.Hashable (Hashable)

type Library a = HashMap (ResolveKey a) (Resolved a)

class (Eq (ResolveKey a), Hashable (ResolveKey a)) => Resolvable a where
    type ResolveKey a
    type Resolvee a
    type Resolved a

    fv :: a -> Resolvee a -> HashSet (ResolveKey a)

    substituteAll :: a -> Library a -> Resolvee a -> (Resolved a)

resolveLibrary :: a -> HashMap (ResolveKey a) (Resolvee a) -> Library a
resolveLibrary = undefined

resolveItem :: a -> Library a -> (Resolvee a) -> (Resolved a)
resolveItem = undefined
