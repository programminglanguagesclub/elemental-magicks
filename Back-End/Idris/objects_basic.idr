module Objects_basic

import Data.Vect
import Data.So
import preliminaries
import bounded
import bounded_then_integer
import integer_then_bounded
import hp

public export data Aliveness = Alive | DeadFresh | DeadStale

public export {-eventually make multischool required to be unique at the type level-}
data MonsterSchools = NoSchools
                    | OneSchool (Fin 6)
                    | TwoSchools (Fin 6) (Fin 6)


public export
record BasicMonster where
 constructor MkBasicMonster
 permanentId : Nat {-equivalence classes of cards. Used for revival-}
 temporaryId : Nat {-Id of a particular card for the game-}
 schools : MonsterSchools
 hp : ((currentHp : Bounded 0 Preliminaries.absoluteUpperBound ** (maxHp : Bounded 0 Preliminaries.absoluteUpperBound ** So (currentHp <= maxHp))), {-baseHp:-} Bounded 0 Preliminaries.absoluteUpperBound)
 attack : TemporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)
 defense : TemporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)
 speed : TemporaryPermanentBase (Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound)
 range : TemporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)
 level : (Bounded 0 9, Bounded 0 9, Bounded 1 9)
 soulPoints : ((Bounded 0 2),(Bounded 1 2))
 aliveness : Aliveness

public export
record BasicSpell where
 constructor MkBasicSpell
 temporaryId : Nat
 school : Fin 6 {-Spells must have exactly one school-}
 level : Bounded 1 9

public export
data BasicCard = BasicSpellCard BasicSpell | BasicMonsterCard BasicMonster
