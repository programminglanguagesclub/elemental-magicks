module Objects

import Data.Vect
import Data.So
import preliminaries

public export data Aliveness = Alive | DeadFresh | DeadStale

public export record Monster where
 constructor MkMonster
 attack : TemporaryPermanentBase Attack
 defense : TemporaryPermanentBase Defense
 speed : TemporaryPermanentBase Speed
 range : TemporaryPermanentBase Range
 level : TemporaryPermanentBase Level
 aliveness : Aliveness
 usedAuto : Bool
 usedStart : Bool
 usedEnd : Bool
 usedSpawn : Bool


public export record Spell where
 constructor MkSpell
 level : Level
 usedSpawn : Bool

public export data Card = SpellCard Spell | MonsterCard Monster

syntax repeat3 [val] = ((val ** Oh),(val ** Oh),(val ** Oh))
syntax monster [attack] [defense] [speed] [range] [level] = MkMonster (repeat3 attack) (repeat3 defense) (repeat3 speed) (repeat3 range) (repeat3 level) Alive False False False False
syntax spell [level] = MkSpell level False

mutant_pig : Monster
mutant_pig = monster 20 0 2 1 3

foo : Card
foo = MonsterCard mutant_pig

public export Board : Type
Board = Vect 9 (Maybe Monster)
public export Hand : Nat -> Type
Hand n = Vect n Card
public export Graveyard : Nat -> Type
Graveyard n = Vect n Card
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
public export record Player (cardsInHand : Nat) (cardsInGraveyard : Nat) where
 {-discard currently being ignored-}
 constructor MkPlayer
 board : Board
 hand : Hand cardsInHand
 graveyard : Graveyard cardsInGraveyard
 spawn : Spawn
 soul : Soul
 thoughts : Thoughts
 knowledge : Knowledge
 token : String

syntax "new" "player" [token] = MkPlayer (Vect.replicate 9 Nothing) [] [] Nothing (Vect.replicate 5 Nothing) (0 ** Oh) (Vect.replicate 6 (0 ** Oh)) token
