module Cyk where

type Table payload = HashMap (Int, Int) (Item payload)

data Item payload =
