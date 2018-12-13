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
