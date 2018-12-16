{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Trees where

import Category

data ParseTree cat payload where
    Leaf :: cat -> payload -> ParseTree cat payload
    Vert :: (Combines cat) => cat -> CombineRule cat -> ParseTree cat payload -> ParseTree cat payload -> ParseTree cat payload

data ParseForest cat payload
    = ParseForest cat [MultiNode cat payload]

data MultiNode cat payload where
    MultiLeaf :: payload -> MultiNode cat payload
    MultiVert :: (Combines cat) => CombineRule cat -> ParseForest cat payload -> ParseForest cat payload -> MultiNode cat payload

enumTrees :: ParseForest cat payload -> [ParseTree cat payload]
enumTrees (ParseForest cat nodes) = foldr (++) [] $ map (extractTrees cat) nodes

extractTrees :: cat -> MultiNode cat payload -> [ParseTree cat payload]
extractTrees cat (MultiLeaf payload) = [Leaf cat payload]
extractTrees cat (MultiVert rule xs ys)
    = [ Vert cat rule x y | x <- enumTrees xs, y <- enumTrees ys ]

instance (Show cat, Show payload, Show (CombineRule cat)) => Show (ParseTree cat payload) where
    show = unlines . showTreeLines

showTreeLines :: (Show cat, Show payload, Show (CombineRule cat)) => ParseTree cat payload -> [String]
showTreeLines (Leaf cat payload) = ["< " <> show cat <> " | " <> show payload <> " >"]
showTreeLines (Vert cat rule left right) =
       [""]
    ++ [ show cat <> " (" <> show rule <> ")" ]
    ++ (map ("  " ++) $ showTreeLines left)
    ++ (map ("  " ++) $ showTreeLines right)
    ++ [""]
