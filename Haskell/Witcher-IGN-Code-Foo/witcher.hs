-- Name: Nicholas Quinn
--
-- Description: This file contains the logic to solve one of IGN's 2019 Code Foo
--              challenges. Please see readme.txt for the problem description.
--
----------------------------------------------------------------------------

import System.IO
import Data.List (lines, sortBy)
import Data.List.Split
import Data.Graph
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
type Helmet     = [Item]
type Chest      = [Item]
type Leggings   = [Item]
type Boots      = [Item]


data Armory = Armory {
    everything :: [Item],
    helmet     :: [Item],
    chest      :: [Item],
    leggings   :: [Item],    
    boots      :: [Item]
} deriving (Show)

class ArmoryClass i where
    getEverything :: i -> [Item]

instance ArmoryClass Armory where
    getEverything i = everything i

--class TempClass a i where
--    addItem :: a -> i -> Armory
    
--instance TempClass Armory Item where

--instance ItemClass ArmoryClass where


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
    in Item {armorType = newArmorType, name = newName, cost = newCost, armorValue = newArmorValue}

--
-- Generate all items from contents in CSV file
--
generateItems :: [[String]] -> [Item]
generateItems all = map generateItem all

printAllItems i = mapM_ print i
{-
findArmorType :: Item -> Armory -> Armory
findArmorType i armory = case (getType i) of
                              "Helmet"   -> helmet armory i
                              "Chest"    -> chest armory i
                              "Leggings" -> leggings armory i
                              "Boots"    -> boots armory i 

fillArmory :: Armory -> Everything -> Armory
fillArmory armory allItems = map (findArmorType armory) allItems 
-}

-- Function to compare two pieces of armor
compareArmorValue x y
    | (getValue x) > (getValue y) = GT
    | (getValue x) < (getValue y) = LT
    | (getValue x) == (getValue y) = compare (getCost x) (getCost y)

-- Sort list of armor by it's value and then cost if they are equal
sortByValue :: [Item] -> [Item]
sortByValue newList = sortBy compareArmorValue newList

-- Sort list by armor type
sortByType :: [Item] -> String -> [Item]
sortByType allItems armorType = 
    let newList = filter isType allItems
    in (reverse . sortByValue) newList
        where isType i = armorType == (getType i)

---------------------------- Main Driver -----------------------------------
--
-- Main Driver
--
main :: IO()
main = do
    contents <- readFile "witcher-inventory.csv"

    -- Convert all contents from file into an array of Items so that it can be used
    -- more efficiently
    let allItems = (generateItems . parseComma . tail . lines) contents
    
    let helmets  = sortByType allItems "Helmet"
    let chests   = sortByType allItems "Chest"
    let leggings = sortByType allItems "Leggings"
    let boots    = sortByType allItems "Boots"

    let emptyArmory = Armory {
                everything = [],
                helmet = [],
                chest = [],
                leggings = [],
                boots = [] 
                }

    printAllItems helmets
    print "------"
    printAllItems chests
    print "------"
    printAllItems leggings
    print "------"
    printAllItems boots


    putStrLn "Program Finished!"