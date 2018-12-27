module Ccg.TypeSystem where

import          LambdaCalculus.LambdaTypes (ApplicativeType(..))
import          Data.Text (Text)

type Name = Text

data WrappedType b
    = Type      (ApplicativeType b)
    | SubType   Name (WrappedType b)
