--
-- Name: Nicholas Quinn
--
-- Description: Main function for Word Game
--
-- Usage: If you are running into an error where WordGameLib cannot be found. In command prompt, run 
--        ghc --make main.hs wordGame.hs
--        main.exe
--

import WordGameLib

main :: IO()
main = outputGrid grid