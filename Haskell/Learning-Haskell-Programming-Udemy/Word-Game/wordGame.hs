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
      outputGrid
    ) where 

outputGrid :: [String] -> IO()
outputGrid grid = putStrLn (formatGrid grid)

-- In this case, formatGrid is an alias for unlines so that our code is more readable
formatGrid :: [String] -> String
formatGrid lines = unlines lines 

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