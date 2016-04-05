module Objects

import Data.Vect
import Data.So
import preliminaries
import skill_dsl

public export data Aliveness = Alive | DeadFresh | DeadStale

public export record Monster where
 constructor MkMonster
 attack : TemporaryPermanentBase Attack
 defense : TemporaryPermanentBase Defense
 speed : TemporaryPermanentBase Speed
 range : TemporaryPermanentBase Range
 level : TemporaryPermanentBase Level
 aliveness : Aliveness
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

public export record Spell where
 constructor MkSpell
 level : Level
 spawnSkill : SkillComponent {-another solution to the circular reference issue would be to represent this part of spells (and similarly for monsters) separately... Indeed I think that is superior.
Can have Monster and Spell without any skills, and then import that into skill dsl. Then this file and skill dsl gets imported into another file that puts adds skills to monsters, spells.
-}

 usedSpawn : Bool

public export data Card = SpellCard Nat Spell | MonsterCard Nat Monster

syntax repeat3 [val] = ((val ** Oh),(val ** Oh),(val ** Oh))
syntax monster [attack] [defense] [speed] [range] [level] = MkMonster (repeat3 attack) (repeat3 defense) (repeat3 speed) (repeat3 range) (repeat3 level) Alive Nothing False Nothing False Nothing False Nothing False [] False
syntax spell [level] = MkSpell level False

mutant_pig : Monster
mutant_pig = monster 20 0 2 1 3

{-
foo : Card
foo = MonsterCard 453 mutant_pig
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
