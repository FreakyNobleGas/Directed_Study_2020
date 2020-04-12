--
-- Name: Nicholas Quinn
--
-- Description: Creating Word Game following the Learning Haskell Programming course on Udemy.com. This Word game
--              is very similar to a word search, where you have a grid of letters, and you try to find words by
--              searching the grid horizontally, vertically, and diagonally. 
--

module WordGameLib
    ( 
      grid,
      languages,
      formatGrid,
      outputGrid,
      findWord,
      findWordInLine
    ) where 

import Data.List (isInfixOf)

type Grid = [String]

outputGrid :: Grid -> IO()
outputGrid grid = putStrLn (formatGrid grid)

-- In this case, formatGrid is an alias for unlines so that our code is more readable. Unlines
-- creates a string from an array of strings and inserts a new line character after each original
-- string. This makes the output to the terminal look much prettier.
formatGrid :: Grid -> String
formatGrid lines = unlines lines 

findWord :: Grid -> String -> Bool
-- For every line in grid, findWordInLine returns whether a word exists horizontally, and then returns
-- a boolean value. If True is returned at least once, `or` will return True.
findWord grid word = 
  -- Also include the lines of the grid in reverse, so we can search the lines backwards horizontally.
  let lines = grid ++ (map reverse grid)
  in or ( map (findWordInLine word) lines)

-- isInfixOf finds if word is contained in line.
-- Example: map (findWorldInLine "HASKELL") grid
findWordInLine :: String -> String -> Bool
findWordInLine word line = word `isInfixOf` line


grid = [ 
        "__C________R___",
        "__SI________U__",
        "__HASKELL____B_",
        "__A__A_____S__Y",
        "__R___B___C____",
        "__PHP____H_____",
        "____S_LREP_____",
        "____I__M_Y__L__",
        "____L_E__T_O___",
        "_________HB____",
        "_________O_____",
        "________CN_____"
       ]

languages = [ 
            "BASIC",
            "COBOL",
            "CSHARP",
            "HASKELL",
            "LISP",
            "PERL",
            "PHP",
            "PYTHON",
            "RUBY",
            "SCHEME"
            ]