-- Name: Nicholas Quinn
--
-- Description: Learning about testing and Quality Assurance
--

{-
    Haskell has many tools designed for testing and quality assurance. An example of a testing
    tool that is built into haskell is the expressive type-system. Haskell's expressive type-system
    enforces statically, and is caught in compilation.

    Open source testing libraries include HUnit and QuickCheck.

    QuickCheck Documentation: https://hackage.haskell.org/package/QuickCheck
-}

import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.List

-- Function that will be tested using QuickCheck
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

-- idempotency: Applying a function twice has the same result as applying it only once
prop_idempotent xs = qsort (qsort xs) == qsort xs

{-- QuickCheck Testing Cases. This is inefficient because we have to write these test cases by hand. If only
    there was a way to automate this process...
prop_idempotent []
prop_idempotent [1,1,1,1]
prop_idempotent [1..100]
prop_idempotent [1,5,2,1,2,0,9]


QuickCheck library comes with a set of data generators that uses the `Arbitrary` typeclass to present a uniform interface
to create pseudo-random data.

This command generates a random list of 10 booleans (Though this command is old, and does not work with my version of QuickCheck)
generate 10 (System.Random.mkStdGen 2) arbitrary :: [Bool]
--}

