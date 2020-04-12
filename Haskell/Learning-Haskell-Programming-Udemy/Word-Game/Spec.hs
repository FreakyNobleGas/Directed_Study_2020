--
-- Name: Nicholas Quinn
--
-- Description: Unit Testing for Word Game using Hspec
--

import Test.Hspec
import WordGameLib
import GridData

main :: IO()
main = hspec $ do
    describe "How to write a test" $ do
        it "Should be able to run tests" $ do
            someString `shouldBe` "someString"