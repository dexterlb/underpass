{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ccg.Modal where

import           GHC.Generics (Generic)
import           Data.Hashable (Hashable)
import           Data.MemoCombinators.Class (MemoTable, table)
import qualified Data.MemoCombinators as Memo
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.Tuple (swap)
import           Data.Functor (($>))

import           Ccg.Category
import           Ccg.Lambda (Compositional(..))

import           Utils.Memoise()
import           Utils.Latex
import           Utils.Maths
import qualified LambdaCalculus.LambdaTypes as T
import qualified LambdaCalculus.Lambda as L
import           Utils.Parsing (Parseable(..), (<|>))
import qualified Utils.Parsing as P

type ModalCategory t = Category (NonTerm t) Slash

data NonTerm t
    = NonTerm   t
    | Variable  Text
    deriving (Eq, Generic)

instance (Parseable t) => Parseable (NonTerm t) where
    parser =   (P.try $ P.operator "$" *> (Variable <$> P.identifier))
           <|>                            (NonTerm  <$> parser      )

instance Functor NonTerm where
    fmap f (NonTerm t)  = NonTerm $ f t
    fmap _ (Variable x) = Variable x

mcmap :: (t1 -> t2) -> ModalCategory t1 -> ModalCategory t2
mcmap f = cmap (f <$>)

instance (MemoTable t) => MemoTable (NonTerm t) where
    table f (NonTerm  x) = table (f . NonTerm) x
    table f (Variable x) = table (f . Variable) x

deriving instance Hashable t => (Hashable (NonTerm t))
instance Show t => Show (NonTerm t) where
    show (NonTerm  t) = show t
    show (Variable t) = T.unpack $ "<" <> t <> ">"

instance (Latexable t) => Latexable (NonTerm t) where
    latex (NonTerm t)  = "$" <> latex t <> "$"
    latex (Variable t) = "$\\$" <> t <> "$"

data Rule
    = LeftApp
    | RightApp
    deriving (Eq, Generic, Show)

deriving instance (Hashable Rule)

instance (HasPrimaryDir Rule) where
    primaryDir LeftApp  = RightPrimary
    primaryDir RightApp = LeftPrimary

data Slash
    = LeftSlash  Modality
    | RightSlash Modality
    deriving (Eq, Generic)
deriving instance (Hashable Slash)

instance Parseable Slash where
    parser = parseSimpleSlash <|> parseModalSlash
        where
            parseSimpleSlash
                =   (P.operator "/"  $> (RightSlash Dot))
                <|> (P.operator "\\" $> (LeftSlash Dot))

            parseModalSlash = (P.lexeme . P.try) $
                    (P.try $ P.char  '/' *> (RightSlash <$> parser) <* P.char  '/')
                <|> (P.try $ P.char '\\' *> (RightSlash <$> parser) <* P.char '\\')

instance MemoTable Slash where
    table f (LeftSlash x) = table (f . LeftSlash) x
    table f (RightSlash x) = table (f . RightSlash) x

data Modality
    = Star
    | Diamond
    | X
    | Dot
    deriving (Eq, Enum, Generic)

instance MemoTable Modality where
    table = Memo.enum

instance PartialOrd Modality where
    Dot     <! _        = True
    X       <! X        = True
    X       <! Star     = True
    Diamond <! Diamond  = True
    Diamond <! Star     = True
    Star    <! Star     = True
    _       <! _        = False

instance MSemiLattice Modality where
    Dot  /\ x   = x
    x    /\ Dot = x
    x    /\ y
      | x == y    = x
      | otherwise = Star

instance Parseable Modality where
    parser
        =   (P.try $ P.char '.' $> Dot)
        <|> (P.try $ P.char 'd' $> Diamond)
        <|> (P.try $ P.char 'x' $> X)
        <|> (P.try $ P.char '*' $> Star)

deriving instance (Hashable Modality)

instance Show Modality where
    show Star    = "★"
    show Diamond = "◆"
    show X       = "⨯"
    show Dot     = "·"

instance Latexable Modality where
    latex Star    = "\\star"
    latex Diamond = "\\Diamond"
    latex X       = "\\times"
    latex Dot     = ""

instance Show Slash where
    show (LeftSlash  m) = "\\" <> show m
    show (RightSlash m) = "/"  <> show m

instance Latexable Slash where
    latex (LeftSlash m)  = "$\\backslash_{" <> latex m <> "}$"
    latex (RightSlash m) = "$/_{"           <> latex m <> "}$"

instance Finite Rule where
    listAll = [LeftApp, RightApp]

instance Latexable Rule where
    latex = T.pack . show

instance (MSemiLattice t) => Combines (ModalCategory t) where
    type CombineRule (ModalCategory t) = Rule
    -- todo: make the categories have disjunct vars before combining
    combineBy rule left right
        |   LeftApp <- rule
          , z <- left', Complex (LeftSlash m) x y <- right'
          , Just (_, r) <- z =<= y, m <! Star
          = Just $ r x
        |   RightApp <- rule
          , Complex (RightSlash m) x y <- left', z <- right'
          , Just (r, _) <- y =>= z, m <! Star
          = Just $ r x
        | otherwise = Nothing
        where
            left'  = addVarSuffix "_l" left  (vars right)
            right' = addVarSuffix "_r" right (vars left)

-- unification

(=>=) :: (MSemiLattice t) => ModalCategory t -> ModalCategory t -> Maybe (ModalCategory t -> ModalCategory t, ModalCategory t -> ModalCategory t)
x =>= y = (\(_, r1, r2) -> (substitute r1, substitute r2)) <$> unifyLeft x y

(=<=) :: (MSemiLattice t) => ModalCategory t -> ModalCategory t -> Maybe (ModalCategory t -> ModalCategory t, ModalCategory t -> ModalCategory t)
x =<= y = swap <$> (y =>= x)

type Substitution t = [(Text, ModalCategory t)]

unifyLeft :: (MSemiLattice t) => ModalCategory t -> ModalCategory t -> Maybe (ModalCategory t, Substitution t, Substitution t)
unifyLeft (Simple (Variable a)) y = Just (y, [(a, y)], [])
unifyLeft x (Simple (Variable a)) = Just (x, [], [(a, x)])
unifyLeft (Simple (NonTerm p)) (Simple (NonTerm q))
    | p !> q    = Just (Simple $ NonTerm $ p /\ q, [], [])
    | otherwise = Nothing
unifyLeft (Complex sl al bl) (Complex sr ar br) = do
    (a, alSub, arSub) <- unifyLeft al ar
    let bl' = substitute alSub bl
    let br' = substitute arSub br
    (b, blSub, brSub) <- unifyLeft br' bl'
    s <- unifySlash sl sr
    pure (Complex s a b, alSub <> blSub, arSub <> brSub)
unifyLeft _ _ = Nothing

unifySlash :: Slash -> Slash -> Maybe Slash
unifySlash (LeftSlash a)  (LeftSlash b)  = Just $ LeftSlash  $ a /\ b
unifySlash (RightSlash a) (RightSlash b) = Just $ RightSlash $ a /\ b
unifySlash _ _ = Nothing


substitute :: Substitution t -> ModalCategory t -> ModalCategory t
substitute subs cat = foldl substituteOne cat subs

substituteOne :: ModalCategory t -> (Text, ModalCategory t) -> ModalCategory t
substituteOne (Complex sl left right) sub =
    Complex sl (substituteOne left sub) (substituteOne right sub)
substituteOne (Simple (Variable b)) (a, x)
    | a == b    = x
    | otherwise = Simple (Variable b)
substituteOne t _ = t

addVarSuffix :: Text -> ModalCategory t -> HashSet Text -> ModalCategory t
addVarSuffix suff (Complex s left right) v = Complex s (addVarSuffix suff left  v)
                                                       (addVarSuffix suff right v)
addVarSuffix suff (Simple (Variable x))  v
    | HS.member x v = Simple $ Variable $ x <> suff
    | otherwise        = Simple $ Variable   x
addVarSuffix _ other _ = other

vars :: ModalCategory t -> HashSet Text
vars (Complex _ left right) = HS.union (vars left) (vars right)
vars (Simple (Variable x))  = HS.singleton x
vars _                      = HS.empty

-- convenience functions

sc :: t -> ModalCategory t
sc s = Simple $ NonTerm $ s

vc :: String -> ModalCategory t
vc s = Simple $ Variable $ T.pack s

(</>) :: ModalCategory t -> ModalCategory t -> ModalCategory t
a </> b = Complex (RightSlash Dot) a b

(<\>) :: ModalCategory t -> ModalCategory t -> ModalCategory t
a <\> b = Complex (LeftSlash Dot) a b


-- lambda instances

instance (T.Typed x t) => T.Typed (NonTerm x) t where
    typeOf (NonTerm x)  = T.typeOf x
    typeOf (Variable _) = T.Wildcard

instance Compositional Rule where
    compose LeftApp  x y = L.Application y x
    compose RightApp x y = L.Application x y
