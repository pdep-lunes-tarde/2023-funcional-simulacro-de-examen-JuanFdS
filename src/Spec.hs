module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "Parcial" $ do
        it "test de prueba" $ do
            2 + 2 `shouldBe` 4
