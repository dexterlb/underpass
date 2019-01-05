{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LambdaCalculus.UserTerms where

import qualified LambdaCalculus.LambdaTypes as T
import           LambdaCalculus.LambdaTypes (Ref(..), Typed(..))
import           LambdaCalculus.Lambda (LambdaTerm)
import qualified LambdaCalculus.Lambda as L
import           LambdaCalculus.UserTypeSystem
import           Utils.Maths

instance (Typed c t) => Typed (Ref c) (Ref t) where
    typeOf (UnresolvedName _ _)  = T.Bot
    typeOf (BasicRef  x)         = T.basicTransform BasicRef $ typeOf x

newtype ConstRef c = ConstRef (Ref c)

deriving instance (Show c) => Show (ConstRef c)
deriving instance (Eq c) => Eq (ConstRef c)
deriving instance (PartialOrd c) => PartialOrd (ConstRef c)

instance (Eq t, PartialOrd t, Typed c t) => Typed (ConstRef c) (TypeWrapper t) where
    typeOf (ConstRef (BasicRef c)) = wrapType' $ typeOf c
    typeOf (ConstRef            _) = T.Bot

resolveTypes :: (Typed c t, Eq t, PartialOrd t) => TypeWrappers t -> LambdaTerm (Ref t) (Ref c) -> LambdaTerm (TypeWrapper t) (ConstRef c)
resolveTypes m = L.transformConst ConstRef (resolveBasicType m)
