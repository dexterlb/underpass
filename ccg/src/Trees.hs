{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Trees where

import Category
import Latex

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

instance (Latexable cat, Latexable payload, Latexable (CombineRule cat), HasPrimaryDir (CombineRule cat))
    => Latexable (ParseTree cat payload) where
    latex tree = "\\vspace{1em}\n\\begin{tikzpicture}\n\\Tree [ " <> latex' tree <> " ]\n\\end{tikzpicture}\n\\vspace{1em}\n\n"
        where
            latex' (Leaf cat payload)
              =  ".{" <> latex cat <> " } { " <> latex payload <> "}"
            latex' (Vert cat rule left right)
              =  ".{ " <> latex cat <> " } "
              <> (if primaryDir rule == LeftPrimary  then "\\edge[very thick];" else "")
              <> " [ " <> latex' left  <> " ] "
              <> (if primaryDir rule == RightPrimary then "\\edge[very thick];" else "")
              <> " [ " <> latex' right <> " ] "

