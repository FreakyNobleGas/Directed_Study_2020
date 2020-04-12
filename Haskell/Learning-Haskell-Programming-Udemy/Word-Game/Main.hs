--
-- Name: Nicholas Quinn
--
-- Description: Main function for Word Game
--
-- Usage: If you are running into an error where WordGameLib cannot be found. In command prompt, run 
--        ghc wordGame.hs
--        ghc Main.hs
--        ghc --make Main.hs wordGame.hs
--        Main.exe
--

import WordGameLib

main :: IO()
main = outputGrid grid