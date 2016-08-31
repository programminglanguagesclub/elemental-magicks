module Card
import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import objects_basic
import skill_dsl_data
import skill_dsl
import phase
import clientupdates
%access public export
%default total

Skill : Type
Skill = (Automatic, Bool, Nat)
{-THIS IS WHERE I CAN REPRESENT EVOKER!
I need to distinguish between abstract skills and skills that have been bound to a particular card.

That is, I want to be able to write skills for cards, and then when I create a card, have the other fields filled.
-}

{- does a skill exist, and has it been used? This way I can also easily make it so that spell cards don't disappear as soon as they are used and monsters don't become engaged as they use their action skills-}

{- used : Bool, cost : Nat -}

record Monster where
 constructor MkMonster
 basic : BasicMonster
 startSkill : Maybe Skill
 endSkill : Maybe Skill
 counterSkill : Maybe Skill
 spawnSkill     : Maybe Skill
 deathSkill      : Maybe Skill
 autoSkill   : Maybe Skill
 actionSkills : List Skill
 
 soulSkill : Maybe Skill


record Spell where
 constructor MkSpell
 basic      : BasicSpell
 spawnSkill : Skill

data Card = SpellCard Spell | MonsterCard Monster


{-
syntax spell [basic] [spawn] = MkSpell basic spawn
-}
{-
mutant_pig : BasicMonster
mutant_pig = basic monster 20 0 2 1 3
-}



