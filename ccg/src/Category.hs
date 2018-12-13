module Category where

data Category atom slash
    = Atom  atom
    | Slash slash (Category atom slash) (Category atom slash)

class Combines rule a where
    combine :: a -> a -> [(rule, a)]

class CombinesBy rule a where
    combineBy :: rule -> a -> a -> Maybe a

instance (CombinesBy rule a, Finite rule) => Combines rule a where
    combine x y = catMaybes $ map (\rule -> (rule,) <$> combineBy rule x y) all

class Finite a where
    all :: [a]

type SimpleCategory = Category String SimpleSlash
data SimpleRule = LeftApp | RightApp deriving (Eq)
data SimpleSlash = Left | Right

instance Finite SimpleRule where
    all = [LeftApp, RightApp]

instance CombinesBy SimpleRule SimpleCategory where
    combine LeftApp (Slash Left x y) z
        | x == z = Just y
        | otherwise = Nothing
    combine RightApp (Slash Right x y) z
        | y == z = Just x
        | otherwise = Nothing


