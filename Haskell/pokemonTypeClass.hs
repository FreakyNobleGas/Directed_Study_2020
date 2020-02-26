-- Name: Nicholas Quinn
--
-- Description: This type class represents all of the 1st Generation Pokemon types and their
--              corresponding strengths and weaknesses.

-- All 1st Generation Pokemon Types
data PokeType = Normal | Fire | Water | Electric | Grass | Ice |
                 Fighting | Poison | Ground | Flying | Physic |
                 Bug | Rock | Ghost | Dragon | Dark | Steel
                deriving (Eq, Ord, Show, Read, Bounded, Enum)
                
data Pokemon = Pokemon {
    typeOfPokemon :: PokeType
}

-- Returns Effectiveness
data Effectiveness = SuperEffective | NormalEffectiveness | NotVeryEffective

-- Define show for various effectiveness
instance Show Effectiveness where
    show SuperEffective = "Super Effective!"
    show NormalEffectiveness = "Normal Effectiveness"
    show NotVeryEffective = "Not Very Effective"

class PokemonEffectiveness a where
    isEffective :: a -> a -> Effectiveness

instance PokemonEffectiveness PokeType where
    isEffective Normal Normal = NormalEffectiveness
    isEffective Grass Fire = SuperEffective