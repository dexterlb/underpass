module Ccg.ModalSpec (main, spec) where

import Test.Hspec
import Ccg.Modal
import Ccg.Category

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "combining" $ do
    it "works on simple examples" $
      combine (sc "S" </> sc "C") (sc "C") `shouldBe` [(RightApp, sc "S")]
    it "works on examples with variables" $ do
      combine (sc "S" </> vc "x" </> vc "x") (sc "C")
        `shouldBe` [(RightApp, sc "S" </> sc "C")]
      combine (sc "A") (sc "S" </> vc "x" <\> vc "x")
        `shouldBe` [(LeftApp, sc "S" </> sc "A")]
      combine (sc "S" </> vc "x" </> vc "x") (sc "C" </> sc "A")
        `shouldBe` [(RightApp, sc "S" </> (sc "C" </> sc "A"))]
      combine (vc "y" </> vc "x" </> vc "x") (sc "C" </> sc "A")
        `shouldBe` [(RightApp, vc "y" </> (sc "C" </> sc "A"))]
    it "renames common variables" $
      combine (sc "S" </> vc "x" </> vc "x") (sc "C" </> vc "x")
        `shouldBe` [(RightApp, sc "S" </> (sc "C" </> vc "x_r"))]
    it "renames overlapping variables" $
      combine (vc "y" </> vc "x" </> vc "x") (vc "y" </> sc "A")
        `shouldBe` [(RightApp, vc "y_l" </> (vc "y_r" </> sc "A"))]
