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
    describe "formatGrid" $ do
        it "Should concatenate every line with a newline" $ do
            (formatGrid ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"