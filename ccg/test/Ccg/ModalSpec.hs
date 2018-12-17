module Ccg.ModalSpec (main, spec) where

import Test.Hspec
import Ccg.Modal
import Ccg.Category

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "combining" $ do
    it "works on simple examples" $ do
      combine (sc "S" </> sc "C") (sc "C") `shouldBe` [(RightApp, sc "S")]
    it "works on examples with variables" $ do
      combine (sc "S" </> vc "x" </> vc "x") (sc "C")
        `shouldBe` [(RightApp, (sc "S" </> sc "C"))]
      combine (sc "A") (sc "S" </> vc "x" <\> vc "x")
        `shouldBe` [(RightApp, (sc "S" </> sc "A"))]
