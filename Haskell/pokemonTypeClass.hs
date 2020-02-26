-- Name: Nicholas Quinn
--
-- Description: This type class represents all of the 1st Generation Pokemon types and their
--              corresponding strengths and weaknesses.
--
-- Link to Table: https://pokemondb.net/type/old
--
-- Usage: `isEffective PokeType PokeType` where the first PokeType is the attacking Pokemon
--        and the second is the defending.
--
-- Example: isEffective Grass Dragon
-- Result:  Not Very Effective!

-- All 1st Generation Pokemon Types
data PokeType = Normal | Fire | Water | Electric | Grass | Ice |
                 Fighting | Poison | Ground | Flying | Psychic |
                 Bug | Rock | Ghost | Dragon | Dark | Steel
                deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Returns Effectiveness
data Effectiveness = SuperEffective | NormalEffectiveness | NotVeryEffective | NoEffect

-- Define show for various effectiveness
instance Show Effectiveness where
    show SuperEffective = "Super Effective!"
    show NormalEffectiveness = "Normal Effectiveness"
    show NotVeryEffective = "Not Very Effective!"
    show NoEffect = "No Effect"

-- Declaration of type class
class PokemonEffectiveness a where
    isEffective :: a -> a -> Effectiveness

-- Format of isEffective is attacking Pokemon first, then defending Pokemon
instance PokemonEffectiveness PokeType where
    -- Normal Type
    isEffective Normal Rock = NotVeryEffective
    isEffective Normal Ghost = NoEffect 
    isEffective Normal _ = NormalEffectiveness

    -- Fire Type
    isEffective Fire Fire = NotVeryEffective
    isEffective Fire Water = NotVeryEffective
    isEffective Fire Grass = SuperEffective
    isEffective Fire Ice = SuperEffective
    isEffective Fire Bug = SuperEffective
    isEffective Fire Rock = NotVeryEffective
    isEffective Fire Dragon = NotVeryEffective
    isEffective Fire _ = NormalEffectiveness

    -- Water Type
    isEffective Water Fire = SuperEffective
    isEffective Water Water = NotVeryEffective
    isEffective Water Grass = NotVeryEffective
    isEffective Water Ground = SuperEffective
    isEffective Water Rock = SuperEffective
    isEffective Water Dragon = NotVeryEffective
    isEffective Water _ = NormalEffectiveness

    -- Electric Type
    isEffective Electric Water = SuperEffective
    isEffective Electric Electric = NotVeryEffective 
    isEffective Electric Grass = NotVeryEffective
    isEffective Electric Ground = NoEffect
    isEffective Electric Flying = SuperEffective
    isEffective Electric Dragon = NotVeryEffective
    isEffective Electric _ = NormalEffectiveness

    -- Grass Type
    isEffective Grass Fire = NotVeryEffective
    isEffective Grass Water = SuperEffective
    isEffective Grass Grass = NotVeryEffective
    isEffective Grass Poison = NotVeryEffective
    isEffective Grass Ground = SuperEffective
    isEffective Grass Flying = NotVeryEffective
    isEffective Grass Bug = NotVeryEffective
    isEffective Grass Rock = SuperEffective
    isEffective Grass Dragon = NotVeryEffective
    isEffective Grass _ = NormalEffectiveness

    -- Ice Type
    isEffective Ice Water = NotVeryEffective
    isEffective Ice Grass = SuperEffective
    isEffective Ice Ice = NotVeryEffective
    isEffective Ice Ground = SuperEffective
    isEffective Ice Flying = SuperEffective
    isEffective Ice Dragon = SuperEffective
    isEffective Ice _ = NormalEffectiveness

    -- Fighting Type
    isEffective Fighting Normal = SuperEffective
    isEffective Fighting Ice = SuperEffective
    isEffective Fighting Poison = NotVeryEffective
    isEffective Fighting Flying = NotVeryEffective
    isEffective Fighting Psychic = NotVeryEffective
    isEffective Fighting Bug = NotVeryEffective
    isEffective Fighting Rock = SuperEffective
    isEffective Fighting Ghost = NoEffect
    isEffective Fighting _ = NormalEffectiveness

    -- Poison Type
    isEffective Poison Grass = SuperEffective
    isEffective Poison Poison = NotVeryEffective
    isEffective Poison Ground = NotVeryEffective
    isEffective Poison Bug = SuperEffective
    isEffective Poison Rock = NotVeryEffective
    isEffective Poison Ghost = NotVeryEffective
    isEffective Poison _ = NormalEffectiveness

    -- Ground Type
    isEffective Ground Fire = SuperEffective
    isEffective Ground Electric = SuperEffective
    isEffective Ground Grass = NotVeryEffective
    isEffective Ground Poison = SuperEffective
    isEffective Ground Flying = NoEffect
    isEffective Ground Bug = NotVeryEffective
    isEffective Ground Rock = SuperEffective
    isEffective Ground _ = NormalEffectiveness

    -- Flying Type
    isEffective Flying Electric = NotVeryEffective
    isEffective Flying Grass = SuperEffective
    isEffective Flying Fighting = SuperEffective
    isEffective Flying Bug = SuperEffective
    isEffective Flying Rock = NotVeryEffective
    isEffective Flying _ = NormalEffectiveness

    -- Psychic Type
    isEffective Psychic Fighting = SuperEffective
    isEffective Psychic Poison = SuperEffective
    isEffective Psychic Psychic = NotVeryEffective
    isEffective Psychic _ = NormalEffectiveness

    -- Bug Type
    isEffective Bug Fire = NotVeryEffective
    isEffective Bug Grass = SuperEffective
    isEffective Bug Fighting = NotVeryEffective
    isEffective Bug Poison = SuperEffective
    isEffective Bug Flying = NotVeryEffective
    isEffective Bug Ghost = NotVeryEffective
    isEffective Bug _ = NormalEffectiveness

    -- Rock Type
    isEffective Rock Fire = SuperEffective
    isEffective Rock Ice = SuperEffective
    isEffective Rock Fighting = NotVeryEffective
    isEffective Rock Ground = NotVeryEffective
    isEffective Rock Flying = SuperEffective
    isEffective Rock Bug = SuperEffective
    isEffective Rock _ = NormalEffectiveness

    -- Ghost Type
    isEffective Ghost Normal = NoEffect
    isEffective Ghost Psychic = NoEffect 
    isEffective Ghost Ghost = SuperEffective
    isEffective Ghost _ = NormalEffectiveness

    -- Dragon Type
    isEffective Dragon Dragon = SuperEffective
    isEffective Dragon _ = NormalEffectiveness