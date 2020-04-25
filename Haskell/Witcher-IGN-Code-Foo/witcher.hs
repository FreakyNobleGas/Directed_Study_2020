-- Name: Nicholas Quinn
--
-- Description: This file contains the logic to solve one of IGN's 2019 Code Foo
--              challenges. Please see readme.txt for the problem description.
--
----------------------------------------------------------------------------

import System.IO

---------------------------- Main Driver -----------------------------------
--
--
--
main :: IO()
main = do
    contents <- readFile "witcher-inventory.csv"

    putStrLn "Program Finished!"