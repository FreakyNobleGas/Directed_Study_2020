-- Name: Nicholas Quinn
--
-- Description: Case study on Barcode Recognition
--

{-
    This case study focuses on the EAN-13 barcode standard, which is a 13-digit sequence broken
    into 4 groups.

    First 2 digits describe the number system, such as the nationality of the manufactor and other
    characteristics.

    Next 5 digits is the manufactor ID

    The next 5 digits is the product ID

    The last digit is the check digit, which allows a scanner to validate the digit string
    it scanned.

    A barcode is a series of fixed width bars, where a black bar represents a 1 and white is 0.
-}

import Data.Array (Array(..), (!), bounds, elems, indices,
                   ixmap, listArray)

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Char (digitToInt)
import Data.Ix (Ix(..))
import Data.List (foldl', group, sort, sortBy, tails)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

-- Imported from Chapter 10
import Parse  

-- Locates the last digit of the barcode to determine it's check digit. The check digit is calculated
-- by taking the difference between the sum of every 3rd number (multiplied by 3 off and on) mod 10 and then
-- subtract 10
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
    where products = mapEveryOther (*3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f,id])