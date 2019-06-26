{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaCalculus.LambdaTypes where

import           Data.Aeson (ToJSON(..), object, (.=))
import qualified Utils.Parsing as P
import Utils.Parsing ((<|>))
import Data.Functor (($>))

import Utils.Exception (Exception, throw)
import Data.Dynamic (Typeable)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Utils.Latex (Latexable, latex)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Utils.Maths
import Data.MemoCombinators.Class (MemoTable, table)
import Data.MemoCombinators (Memo, memo2)

data ApplicativeType b
    = Basic b
    | Application (ApplicativeType b) (ApplicativeType b)
    | Wildcard
    deriving (Eq, Generic)

deriving instance (Hashable b) => Hashable (ApplicativeType b)

class (Show b, Show a, Typeable a, Typeable b, MLattice (ApplicativeType b)) => Typed a b where  -- items of haskell type a have basic types from b
    typeOf :: a -> ApplicativeType b

instance (Show t, Typeable t, MLattice (ApplicativeType t)) => Typed (ApplicativeType t) t where
    typeOf = id

instance (Show b) => Show (ApplicativeType b) where
    show (Basic x) = show x
    show (Application a b) = "(" <> show a <> " -> " <> show b <> ")"
    show Wildcard = "*"

instance (P.Parseable b) => P.Parseable (ApplicativeType b) where
    parser = parseTypeExpr

instance Functor ApplicativeType where
    fmap f (Basic x)         = Basic $ f x
    fmap f (Application a b) = Application (fmap f a) (fmap f b)
    fmap _ Wildcard               = Wildcard

parseTypeExpr :: (P.Parseable b) => P.Parser (ApplicativeType b)
parseTypeExpr = P.makeExprParser parseTypeTerm
    [ [ P.InfixR ((P.operator "->" <|> P.operator "→") $> Application) ]
    ]

parseTypeTerm :: (P.Parseable b) => P.Parser (ApplicativeType b)
parseTypeTerm
    =   P.braces parseTypeExpr
    <|> (Wildcard   <$  P.operator "*")
    <|> (Basic <$> P.parser)

inferenceUnifier :: (MSemiLattice (ApplicativeType b)) => ApplicativeType b -> ApplicativeType b -> ApplicativeType b
inferenceUnifier (Application a1 a2) (Application b1 b2) = Application (inferenceUnifier a1 b1) (inferenceUnifier a2 b2)
inferenceUnifier Wildcard x = x
inferenceUnifier x Wildcard = x
inferenceUnifier x y
    | x <! y = x
    | x !> y = y
    | otherwise = throw $ CannotMeet x y

defaultMeet :: MLattice b => ApplicativeType b -> ApplicativeType b -> ApplicativeType b
defaultMeet (Basic x) (Basic y) = Basic $ x /\ y
defaultMeet (Application a1 a2) (Application b1 b2) = Application (defaultJoin a1 b1) (defaultMeet a2 b2)
defaultMeet Wildcard x = x
defaultMeet x Wildcard = x
defaultMeet x y = throw $ CannotMeet x y

defaultJoin :: MLattice b => ApplicativeType b -> ApplicativeType b -> ApplicativeType b
defaultJoin (Basic x) (Basic y) = Basic $ x \/ y
defaultJoin (Application a1 a2) (Application b1 b2) = Application (defaultMeet a1 b1) (defaultJoin a2 b2)
defaultJoin Wildcard _ = Wildcard
defaultJoin _ Wildcard = Wildcard
defaultJoin x y = throw $ CannotJoin x y

defaultPartialMeet :: (Eq b, Show b, Typeable b) => ApplicativeType b -> ApplicativeType b -> ApplicativeType b
defaultPartialMeet (Basic x) (Basic y)
  | x == y    = Basic x
  | otherwise = throw $ CannotMeet (Basic x) (Basic y)
defaultPartialMeet (Application a1 a2) (Application b1 b2) = Application (defaultPartialJoin a1 b1) (defaultPartialMeet a2 b2)
defaultPartialMeet Wildcard x = x
defaultPartialMeet x Wildcard = x
defaultPartialMeet x y = throw $ CannotMeet x y

defaultPartialJoin :: (Eq b, Show b, Typeable b) => ApplicativeType b -> ApplicativeType b -> ApplicativeType b
defaultPartialJoin (Basic x) (Basic y)
  | x == y    = Basic x
  | otherwise = throw $ CannotJoin (Basic x) (Basic y)
defaultPartialJoin (Application a1 a2) (Application b1 b2) = Application (defaultPartialMeet a1 b1) (defaultPartialJoin a2 b2)
defaultPartialJoin Wildcard x = x
defaultPartialJoin x Wildcard = x
defaultPartialJoin x y = throw $ CannotJoin x y


-- be wary of the contravariance!
defaultLess :: PartialOrd b => ApplicativeType b -> ApplicativeType b -> Bool
defaultLess (Basic x)           (Basic y)           = x <! y
defaultLess (Application a1 a2) (Application b1 b2) = defaultLess b1 a1 && defaultLess a2 b2
defaultLess Wildcard    Wildcard          = True
defaultLess Wildcard    _            = True
defaultLess _      Wildcard          = True
defaultLess _      _            = False

inferApp :: (MSemiLattice (ApplicativeType t)) => ApplicativeType t -> (ApplicativeType t, ApplicativeType t)
inferApp x
    | (Application p q) <- x `inferenceUnifier` (Application Wildcard Wildcard) = (p, q)
    | otherwise = error "how did x unify to * -> * but the result is not T -> T ?"

instance HasBot (ApplicativeType b) where
    bot = Wildcard

basicTransform :: (t1 -> t2) -> ApplicativeType t1 -> ApplicativeType t2
basicTransform f = transform (Basic . f)

transform :: (t1 -> ApplicativeType t2) -> ApplicativeType t1 -> ApplicativeType t2
transform f (Basic x)           = f x
transform f (Application a b)   = Application (transform f a) (transform f b)
transform _ Wildcard                 = Wildcard

data TypeException t
    = CannotMeet t t
    | CannotJoin t t
    | WrongLambdaType t
    | CannotApply t t
    deriving (Typeable)

deriving instance Show t => Show (TypeException t)

instance (Show t, Typeable t) => Exception (TypeException t)

instance Latexable a => Latexable (ApplicativeType a) where
    latex (Basic x)         = latex x
    latex (Application a b) = latex a <> " \\rightarrow " <> latex b
    latex Wildcard               = "*"

apply :: (MSemiLattice (ApplicativeType t)) => ApplicativeType t -> ApplicativeType t -> ApplicativeType t
apply f arg
    | arg <! targ = tret
    | otherwise   = throw $ CannotApply f arg
    where
        (targ, tret) = inferApp f




-- memo instances
instance (MemoTable t) => MemoTable (ApplicativeType t) where
    table = mkmemo (table :: Memo t)
        where
            mkmemo :: forall t' b.
                      (forall a. (t' -> a) -> (t' -> a))
                   -> (ApplicativeType t' -> b)
                   -> (ApplicativeType t' -> b)
            mkmemo mt f (Basic x) = mt (f . Basic) x
            mkmemo mt f (Application x y) =
                memo2 mapp mapp (\a b -> f $ Application a b) x y
                where
                    mapp :: (ApplicativeType t' -> s) -> (ApplicativeType t' -> s)
                    mapp = mkmemo mt
            mkmemo _  f Wildcard = f Wildcard

-- parsing helpers
type Name = Text

data Ref t            = UnresolvedName P.SourcePos Name | BasicRef t
type UnresolvedType t = ApplicativeType (Ref t)

unresolvedNames :: UnresolvedType t -> HashSet Name
unresolvedNames (Basic (UnresolvedName _ name)) = HS.singleton name
unresolvedNames (Application a b) = HS.union (unresolvedNames a) (unresolvedNames b)
unresolvedNames _                 = HS.empty

instance (P.Parseable t) => P.Parseable (Ref t) where
    parser = (BasicRef <$> P.parser) <|> (do
        pos  <- P.getSourcePos
        name <- P.identifier
        pure $ UnresolvedName pos name
        )

instance (Show t) => Show (Ref t) where
    show (BasicRef x) = show x
    show (UnresolvedName _ name) = "<unresolved " <> Text.unpack name <> ">"

deriving instance (Eq t) => Eq (Ref t)

instance (PartialOrd t) => PartialOrd (Ref t) where
    (BasicRef x) <! (BasicRef y) = x <! y
    _            <! _            = False

instance (PartialOrd (ApplicativeType t)) => PartialOrd (ApplicativeType (Ref t)) where
    a <! b = stripRefs a <! stripRefs b

instance (Show t, Typeable t, MSemiLattice (ApplicativeType t)) => MSemiLattice (ApplicativeType (Ref t)) where
    a /\ b
        | refless a, refless b = BasicRef <$> (stripRefs a /\ stripRefs b)
        | otherwise = error "meet for types with unresolved components not yet implemented"

instance (Show t, Typeable t, MLattice (ApplicativeType t)) => MLattice (ApplicativeType (Ref t)) where
    a \/ b
        | refless a, refless b = BasicRef <$> (stripRefs a \/ stripRefs b)
        | otherwise = error "meet for types with unresolved components not yet implemented"

stripRefs :: ApplicativeType (Ref t) -> ApplicativeType t
stripRefs (Basic (BasicRef x)) = Basic x
stripRefs (Application a b) = Application (stripRefs a) (stripRefs b)
stripRefs _ = Wildcard

refless :: ApplicativeType (Ref t) -> Bool
refless (Basic (BasicRef _)) = True
refless (Basic (UnresolvedName _ _)) = False
refless (Application a b) = refless a && refless b
refless _ = True

instance (ToJSON t) => ToJSON (ApplicativeType t) where
    toJSON (Application m n) = object ["_t" .= ("application" :: Text), "left" .= m, "right" .= n]
    toJSON (Basic m)         = object ["_t" .= ("basic" :: Text),       "type" .= m]
    toJSON Wildcard          = object ["_t" .= ("wildcard" :: Text)]
