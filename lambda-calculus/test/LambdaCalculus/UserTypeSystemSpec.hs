{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module LambdaCalculus.UserTypeSystemSpec (main, spec) where

import Test.Hspec

import LambdaCalculus.LambdaTypes
import Utils.Maths

import LambdaCalculus.UserTypeSystem

main :: IO ()
main = hspec spec

data Base = E | F | R | S deriving (Show, Eq)

instance PartialOrd Base where
    (<!) = (==)

instance MSemiLattice Base where
    a /\ b
        | a == b    = a
        | otherwise = error "trying to meet unequal base items"

instance PartialOrd (ApplicativeType Base) where
    (<!) = defaultLess

instance MSemiLattice (ApplicativeType Base) where
    (/\) = defaultMeet

tA :: AppTypeWrapper Base
tA = Basic $ SubType "A" tB
tB :: AppTypeWrapper Base
tB = Basic $ SubType "B" (Application tC tD)
tC :: AppTypeWrapper Base
tC = Basic $ SubType "C" tE
tD :: AppTypeWrapper Base
tD = Basic $ SubType "D" tF
tE :: AppTypeWrapper Base
tE = Basic $ Type E
tF :: AppTypeWrapper Base
tF = Basic $ Type F
tP :: AppTypeWrapper Base
tP = Basic $ SubType "P" (Application tR tS)
tR :: AppTypeWrapper Base
tR = Basic $ Type R
tS :: AppTypeWrapper Base
tS = Basic $ Type S

shouldLt :: AppTypeWrapper Base -> AppTypeWrapper Base -> Expectation
shouldLt a b = shouldSatisfy (a, b) $ uncurry (<!)

shouldNotLt :: AppTypeWrapper Base -> AppTypeWrapper Base -> Expectation
shouldNotLt a b = shouldNotSatisfy (a, b) $ uncurry (<!)

shouldGt :: AppTypeWrapper Base -> AppTypeWrapper Base -> Expectation
shouldGt = flip shouldLt

shouldNotGt :: AppTypeWrapper Base -> AppTypeWrapper Base -> Expectation
shouldNotGt = flip shouldNotLt

checkLt :: AppTypeWrapper Base -> AppTypeWrapper Base -> Expectation
checkLt a b = do
    a == b `shouldBe` False
    a `shouldLt` b
    a `shouldNotGt` b
    a /\ b `shouldBe` a

checkEq :: AppTypeWrapper Base -> AppTypeWrapper Base -> Expectation
checkEq a b = do
    a == b `shouldBe` True
    a `shouldLt` b
    a `shouldGt` b

    a /\ b `shouldBe` a
    a /\ b `shouldBe` b

checkUnrelated :: AppTypeWrapper Base -> AppTypeWrapper Base -> Expectation
checkUnrelated a b = do
    a == b `shouldBe` False
    a `shouldNotLt` b
    a `shouldNotGt` b


spec :: Spec
spec = do
  describe "meet and less-than" $ do
    it "works on simple examples" $ do
      checkLt tA tB
      checkLt tC tE
      checkLt tD tF
      checkEq tB tB
      checkEq tP tP
    it "works on examples with application" $ do
      checkLt tA (Application tC tD)
      checkEq (Application tA tA) (Application tA tA)
    it "works on examples with transitive application" $ do
      checkLt tA (Application tE tF)
    it "works on examples with parallel application" $ do
      checkLt (Application tA tP) (Application (Application tE tF) (Application tR tS))
    it "works on examples with *" $ do
      checkLt (Application tA tP) (Application (Application tE tF) (Application tR Wildcard))
  describe "less-than in unrelated cases" $ do
    it "works on simple example" $ do
      checkUnrelated tA tP
      checkUnrelated tB tR
  describe "meet in non-branch cases" $ do
    it "works on example with two *'s" $ do
      (Application Wildcard tP) /\ (Application (Application tE tF) (Application tR Wildcard))
        `shouldBe`
            (Application (Application tE tF) tP)
