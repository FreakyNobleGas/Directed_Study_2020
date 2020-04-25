-- Name: Nicholas Quinn
--
-- Description: This file contains the logic to solve one of IGN's 2019 Code Foo
--              challenges. Please see readme.txt for the problem description.
--
----------------------------------------------------------------------------

import System.IO
import Data.List (lines)
import Data.List.Split
import Text.XML.HXT.DOM.Util

---------------------------- Classes and Data Types ------------------------
--
-- Class to represent data stored in Items
--
class ItemClass i where
    getType  :: i -> String
    getName  :: i -> String
    getCost  :: i -> Int
    getValue :: i -> Int

instance ItemClass Item where
    getType  i = armorType i
    getName  i = name i
    getCost  i = cost i
    getValue i = armorValue i

--
-- Data type that holds information about each item
--
data Item = Item {
    armorType  :: String,
    name       :: String,
    cost       :: Int,
    armorValue :: Int
} deriving (Show)

--
-- Type synonym's for each type of armor to make code more readable
--
type Everything = [Item]
type Chest      = [Item]
type Leggings   = [Item]
type Helmet     = [Item]
type Boots      = [Item]

---------------------------- Helper Functions ------------------------------
--
-- Splits each comma seperated value into it's own [String]. This means every [String]  
-- represents a specific item in the armory
--
parseComma :: [String] -> [[String]]
parseComma p = map (splitOn ",") p

--
-- Generate Item from String value
--
generateItem :: [String] -> Item
generateItem i = 
    let newArmorType   = i !! 0
        newName        = i !! 1
        newCost        = decimalStringToInt $ i !! 2
        newArmorValue  = decimalStringToInt $ i !! 3
    in Item {armorType = newArmorType, name = newName, cost =  newCost, armorValue =  newArmorValue}

--
-- Generate all items from contents in CSV file
--
generateItems :: [[String]] -> [Item]
generateItems armory = map generateItem armory

printAllItems i = mapM_ print i
                            
---------------------------- Main Driver -----------------------------------
--
-- Main Driver
--
main :: IO()
main = do
    contents <- readFile "witcher-inventory.csv"

    -- Convert all contents from file into an array of Items so that it can be used
    -- more efficiently
    let everything = (generateItems . parseComma . tail . lines) contents

    printAllItems everything

    putStrLn "Program Finished!"