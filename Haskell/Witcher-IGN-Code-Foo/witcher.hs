-- Name: Nicholas Quinn
--
-- Description: This file contains the logic to solve one of IGN's 2019 Code Foo
--              challenges. Please see readme.txt for the problem description.
--
----------------------------------------------------------------------------

import System.IO
import Data.List (lines)
import Data.List.Split

---------------------------- Classes and Data Types ------------------------
--
-- Data type that holds information about each item
--

data Item = Item {
    armorType :: String,
    name :: String,
    cost :: Int,
    armorValue :: Int
}

--
-- Type synonym's for each type of armor to make code more readable
--

type Everything = [Item]
type Chest = [Item]
type Leggings = [Item]
type Helmet = [Item]
type Boots = [Item]

---------------------------- Helper Functions ------------------------------
--
-- Splits each comma seperated value into it's own [String]. This means every [String]  
-- represents a specific item in the armory
--
parseComma :: [String] -> [[String]]
parseComma p = map (splitOn ",") p

---------------------------- Main Driver -----------------------------------
--
--
--
main :: IO()
main = do
    contents <- readFile "witcher-inventory.csv"

    -- Remove header from CSV and convert contents into [String]
    let armory = (parseComma . tail . lines) contents
    print armory
    putStrLn "Program Finished!"