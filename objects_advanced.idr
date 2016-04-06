module Objects_advanced

import Data.Vect
import Data.So
import preliminaries
import objects_basic
import skill_dsl



public export
record Monster where
 constructor MkMonster
 basic : BasicMonster
 autoSkill : Maybe SkillComponent
 usedAuto : Bool
 startSkill : Maybe SkillComponent
 usedStart : Bool
 endSkill : Maybe SkillComponent
 usedEnd : Bool
 spawnSkill : Maybe SkillComponent
 usedSpawn : Bool
 actionSkills : List SkillComponent
 usedAction : Bool {-This allows us to make monsters engaged AFTER they FINISH (one of ... s) their action skill -}

public export
record Spell where
 constructor MkSpell
 basic : BasicSpell
 spawnSkill : SkillComponent {-don't need used spawn as I'll have spell cards disappear as soon as they are cast, rather than after their effect goes off. (So might want to also engage units as soon as their action skills trigger. That would also be easier)-}

public export data Card = SpellCard Spell | MonsterCard Monster

{-
syntax monster [basic] [auto] [start] [end] [spawn] [actions] = MkMonster basic auto False start False end False spawn False actions False
syntax spell [basic] [spawn] = MkSpell basic spawn
-}
{-
mutant_pig : BasicMonster
mutant_pig = basic monster 20 0 2 1 3
-}





public export Board : Type
Board = Vect 9 (Maybe Monster)
public export Spawn : Type
Spawn = Maybe Card
public export Soul : Type
Soul = Vect 5 (Maybe Monster) {- again more information could go in the type -}
public export Thoughts : Type
Thoughts = Bounded 0 absoluteUpperBound
public export Knowledge : Type
Knowledge = Vect 6 (Level)

{- A goes first in the first round; B goes first in the second round; -}
{- a lot of types could be made more precise here -}
public export
record Player where
 {-discard currently being ignored-}
 constructor MkPlayer
 board : Board
 hand : List Card
 graveyard : List Card
 discard : List Card
 spawn : Spawn
 soul : Soul
 thoughts : Thoughts
 knowledge : Knowledge
 token : String

syntax "new" "player" [token] = MkPlayer (Vect.replicate 9 Nothing) [] [] [] Nothing (Vect.replicate 5 Nothing) (0 ** Oh) (Vect.replicate 6 (0 ** Oh)) token
