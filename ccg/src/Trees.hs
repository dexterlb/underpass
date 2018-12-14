module Trees where

import Category

data (Combines cat) => ParseTree cat payload
    = Leaf cat payload
    | Vert cat (Rule cat) (ParseTree cat payload) (ParseTree cat payload)

data (Combines cat) => ParseForest cat payload
    = ParseForest cat [MultiNode cat payload]

data (Combines cat) => MultiNode cat payload
    = MultiLeaf payload
    | MultiVert (Rule cat) (ParseForest cat payload) (ParseForest cat payload)

enumTrees :: (Combines cat) => ParseForest cat payload -> [ParseTree cat payload]
enumTrees (ParseForest cat nodes) = foldr (++) [] $ map (extractTrees cat) nodes

extractTrees :: (Combines cat) => cat -> MultiNode cat payload -> [ParseTree cat payload]
extractTrees cat (MultiLeaf payload) = [Leaf cat payload]
extractTrees cat (MultiVert rule xs ys)
    = [ Vert cat rule x y | x <- enumTrees xs, y <- enumTrees ys ]
