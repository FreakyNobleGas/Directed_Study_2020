-- Name: Nicholas Quinn
--
-- Description: This file contains the definition of a Pokemon
--

import System.IO

-- All 1st Generation Pokemon Types
data PokeType = Normal | Fire | Water | Electric | Grass | Ice |
                 Fighting | Poison | Ground | Flying | Psychic |
                 Bug | Rock | Ghost | Dragon | Dark | Steel
                deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- This class allows each unique Pokemon to access it's various traits
-- and/or set them
class UniquePokemon p where
    getType :: p -> PokeType

-- Return Pokemon's Type
instance UniquePokemon Pokemon where
    getType p = pokeType p

-- Pokemon Data Type
data Pokemon = Pokemon {
    name :: String,
    index :: Int,
    pokeType :: PokeType
}

bulbasaur = Pokemon {
    name = "Bulbasaur",
    index = 1,
    pokeType = Grass
}

main = do
    handle <- openFile "ListOfPokemon.csv" ReadMode
    contents <- hGetContents handle  
    putStr contents  
    hClose handle 