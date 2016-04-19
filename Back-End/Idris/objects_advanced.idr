module Objects_advanced

import Data.Vect
import Data.So
import preliminaries
import objects_basic
import skill_dsl


public export
Skill : Type
Skill = (SkillComponent, Bool, Nat) {- does a skill exist, and has it been used? This way I can also easily make it so that spell cards don't disappear as soon as they are used and monsters don't become engaged as they use their action skills-}

{- used : Bool, cost : Nat -}

public export
record Monster where
 constructor MkMonster
 basic        : BasicMonster
 autoSkill    : Maybe Skill
 startSkill   : Maybe Skill
 endSkill     : Maybe Skill
 usedEnd      : Maybe Skill
 spawnSkill   : Maybe Skill
 actionSkills : List Skill

public export
record Spell where
 constructor MkSpell
 basic      : BasicSpell
 spawnSkill : Skill

public export data Card = SpellCard Spell | MonsterCard Monster

{-
syntax monster [basic] [auto] [start] [end] [spawn] [actions] = MkMonster basic auto False start False end False spawn False actions False
syntax spell [basic] [spawn] = MkSpell basic spawn
-}
{-
mutant_pig : BasicMonster
mutant_pig = basic monster 20 0 2 1 3
-}





public export
Board : Type
Board = Vect 9 (Maybe Monster)
public export
Spawn : Type
Spawn = Maybe Card
public export
Soul : Type
Soul = Vect 5 (Maybe Monster) {- again more information could go in the type -}
public export
Thoughts : Type
Thoughts = Bounded 0 absoluteUpperBound
public export
Knowledge : Type
Knowledge = Vect 6 (Level)

public export
record Player where
 constructor MkPlayer
 board           : Vect 9 (Maybe Monster)
 rowTarget       : Vect 3 (Fin 3)
 hand            : List Card
 graveyard       : List Card
 discard         : List Card
 spawn           : Spawn
 soul            : Soul
 thoughts        : Thoughts
 knowledge       : Knowledge
 token           : String






transformRowTarget : Fin 3 -> Vect 3 (Maybe Monster) -> Fin 3
transformRowTarget f m with (f)
 | FZ = ?g {-FS FZ-}
 | FS FZ = FS (FS FZ)
 | FS (FS FZ) = FZ


{-
This doesn't quite work. I need to move to the next position AFTER the next card....
-}

{-

goToNextRowTarget : Player -> Fin 3 -> Player
goToNextRowTarget player n = case n of
                              FZ => record {rowTarget = replaceAt FZ (transformRowTarget) (rowTarget player))(rowTarget player)} player
                              FS FZ => record {rowTarget = (rowTarget player)} player
                              FS (FS FZ) => record {rowTarget = (rowTarget player)} player

-}


{-
goToNextRowTarget player n = case n of with (take 3 (drop (3)(board player)))
 | _ = player
-}
{-

foo : Int -> Int
foo x = case isLT of
            Yes => x*2
            No => x*4
    where
       data MyLT = Yes | No

       isLT : MyLT
       isLT = if x < 20 then Yes else No



-}




syntax "new" "player" [token] = MkPlayer (Vect.replicate 9 Nothing) [] [] [] Nothing (Vect.replicate 5 Nothing) (0 ** Oh) (Vect.replicate 6 (0 ** Oh)) token
