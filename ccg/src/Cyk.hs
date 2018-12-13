module Cyk where

type Table rule cat payload = HashMap (Int, Int, cat) (Item rule cat payload)

data Item rule cat payload
    = Leaf payload
    | Vert rule (Int, Int, cat) (Int, Int, cat)
