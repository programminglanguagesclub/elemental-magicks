module Card_list
import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import hp
import preliminaries
import objects_basic
import skill_dsl_data
import skill_dsl
import phase
import clientupdates
import player
%access public export
%default total


{-temporary ids really make absolutely no sense here at all.
 that needs to be removed from this...
 the cards in the game need to be this plus a temporary id I guess...
 for now I'm just making all of the temporaryIds 0.
-}

{-I should make the syntax require saying the name of the stat before its value...-}


{-not giving mutant pig a skill quite yet-}

t : BasicMonster
t = mkBasicMonster "mutant pig" 0 0 (TwoSchools 4 5) 60 30 0 2 1 3 2
{-
monsterList : List Monster
monsterList = [
 MkMonster (mkBasicMonster "mutant pig" 0 0 (TwoSchools 4 5) 60 30 0 2 1 3 2) Nothing Nothing Nothing Nothing Nothing Nothing []














]-}
