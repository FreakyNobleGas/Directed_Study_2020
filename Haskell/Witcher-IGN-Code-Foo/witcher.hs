-- Name: Nicholas Quinn
--
-- Description: This file contains the logic to solve one of IGN's 2019 Code Foo
--              challenges. Please see readme.txt for the problem description.
--
----------------------------------------------------------------------------

import System.IO
import Data.List (lines, sortBy)
import Data.List.Split
import Data.Maybe (mapMaybe, fromMaybe)
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
    getExtra :: i -> Bool

instance ItemClass Item where
    getType  i = armorType i
    getName  i = name i
    getCost  i = cost i
    getValue i = armorValue i
    getExtra i = extra i

--
-- Data type that holds information about each item
--
data Item = Item {
    armorType  :: String,
    name       :: String,
    cost       :: Int,
    armorValue :: Int,
    extra      :: Bool
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
        newExtra       = False
    in Item {armorType = newArmorType, name = newName, cost = newCost, armorValue = newArmorValue, extra = newExtra}

changeToExtra :: [Item] -> [Item]
changeToExtra allItems = map changeExtraBool allItems
                            where changeExtraBool i = Item {
                                armorType = getType i,
                                name = getName i,
                                cost = getCost i,
                                armorValue = getValue i,
                                extra = True
                            }

--
-- Generate all items from contents in CSV file
--
generateItems :: [[String]] -> [Item]
generateItems all = map generateItem all

printAllItems :: [Item] -> IO()
printAllItems i = mapM_ print i

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

calculateCost :: Item -> Item -> Item -> Item -> Item -> Int
calculateCost helmet chest legging boot extraItem =
    (getCost helmet) + (getCost chest) + (getCost legging) + (getCost boot) + (getCost extraItem)

generateListOfItems :: [(String, Int)] -> [Item] -> [Item] -> [Item] -> [Item] -> [Item] -> [Item]
generateListOfItems selectedItems helmets chests leggings boots extraItems = [(helmets  !! fromMaybe 0 (lookup "helmet" selectedItems)),
                                                                             (chests   !! fromMaybe 0 (lookup "chest" selectedItems)),
                                                                             (leggings !! fromMaybe 0 (lookup "leggings" selectedItems)),
                                                                             (boots    !! fromMaybe 0 (lookup "boots" selectedItems)),
                                                                             (extraItems !! fromMaybe 0 (lookup "extraItem" selectedItems))]

--updateInventory :: [(String, Int)] -> [Item] -> [Item] -> [Item] -> [Item] -> [Item] -> [(String, Int)]
updateInventory :: [(String, Int)] -> [Item] -> [Item] -> [Item] -> [Item] -> [Item] -> Item
updateInventory selectedItems helmets chests leggings boots extraItems = 
    let newList = map incrementVal selectedItems
    in (head . reverse . sortByValue) (generateListOfItems newList helmets chests leggings boots extraItems)
        where incrementVal i = (fst i, (snd i) + 1)

calculateResult :: Int -> [(String, Int)] -> [Item] -> [Item] -> [Item] -> [Item] -> [Item] -> [(String, Int)]
--calculateResult :: Int -> [(String, Int)] -> [Item] -> [Item] -> [Item] -> [Item] -> [Item] -> [Item]
calculateResult cost selectedItems helmets chests leggings boots extraItems = 
    --if cost <= 300 
    --then do selectedItems
    --else do 
    let newItem = updateInventory selectedItems helmets chests leggings boots extraItems
    in if (getExtra newItem)
       then [("helmet", fromMaybe 0 (lookup "helmet" selectedItems)),
             ("chest",  fromMaybe 0 (lookup "chest" selectedItems)),
             ("leggings",  fromMaybe 0 (lookup "leggings" selectedItems)),
             ("boots",  fromMaybe 0 (lookup "boots" selectedItems)),
             ("extraItem",  (fromMaybe 0 (lookup "extraItem" selectedItems)) + 1)
            ] 
        else selectedItems


---------------------------- Main Driver -----------------------------------
--
-- Main Driver
--
main :: IO()
main = do
    contents <- readFile "witcher-inventory.csv"

    -- Convert all contents from file into an array of Items so that it can be used
    -- more efficiently
    let allItems = (reverse . sortByValue . generateItems . parseComma . tail . lines) contents
    
    let helmets  = sortByType allItems "Helmet"
    let chests   = sortByType allItems "Chest"
    let leggings = sortByType allItems "Leggings"
    let boots    = sortByType allItems "Boots"
    let extraItems = changeToExtra allItems
    
    let selectedItems = [("helmet", 0), ("chest", 0), ("leggings", 0), ("boots", 0), ("extraItem", 0)]

    let cost = calculateCost (helmets  !! fromMaybe 0 (lookup "helmet" selectedItems))
                             (chests   !! fromMaybe 0 (lookup "chest" selectedItems))
                             (leggings !! fromMaybe 0 (lookup "leggings" selectedItems))
                             (boots    !! fromMaybe 0 (lookup "boots" selectedItems))
                             (extraItems !! fromMaybe 0 (lookup "extraItem" selectedItems))
    
    let result = calculateResult cost
                                 selectedItems
                                 helmets
                                 chests
                                 leggings
                                 boots
                                 extraItems
                {-                             
    calculateResult = do
                        if cost <= 300 
                        then do putStrLn "Found Result!" 
                        else putStrLn "Need to keep looking"
    -}
    putStrLn "---RESULT---"
    --printAllItems result
    print result
    putStrLn "---COST---"
    print cost
    --print (helmets  !! fromMaybe 0 (lookup "helmet" selectedItems))
    --print (chests   !! fromMaybe 0 (lookup "chest" selectedItems))
    --print (leggings !! fromMaybe 0 (lookup "leggings" selectedItems))
    --print (boots    !! fromMaybe 0 (lookup "boots" selectedItems))
    --print (allItems !! fromMaybe 0 (lookup "extraItem" selectedItems))

    putStrLn "Program Finished!"