{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils.Resolver where

-- import qualified Data.HashMap.Lazy as HM
import           Data.HashMap.Lazy (HashMap)
import           Data.Hashable (Hashable)

class (Eq (ResolveKey a), Hashable (ResolveKey a)) => Resolvable a where
    type ResolveKey a
    type Resolvee a
    type Resolved a

    fv :: a -> Resolvee a -> [ResolveKey a]

    substituteAll :: a -> HashMap (ResolveKey a) (Resolved a) -> Resolvee a -> (Resolved a)

resolveLibrary :: a -> HashMap (ResolveKey a) (Resolvee a) -> HashMap (ResolveKey a) (Resolved a)
resolveLibrary = undefined

resolveItem :: a -> HashMap (ResolveKey a) (Resolved a) -> (Resolvee a) -> (Resolved a)
resolveItem = undefined
