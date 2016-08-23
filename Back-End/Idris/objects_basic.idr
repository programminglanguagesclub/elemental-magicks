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

{-Shouldn't TemporaryPermanentBase be lower case??-}
public export
record BasicMonster where
 constructor MkBasicMonster
 permanentId : Nat {-equivalence classes of cards. Used for revival-}
 temporaryId : Nat {-Id of a particular card for the game-}
 schools : MonsterSchools
 hp : Hp
 attack : temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)
 defense : temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)
 speed : (Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound, Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound, Bounded 1 5)
 range : (Bounded 0 Preliminaries.absoluteUpperBound, Bounded 0 Preliminaries.absoluteUpperBound, Bounded 1 5)
 level : (Bounded 0 9, Bounded 0 9, Bounded 1 9)
 soulPoints : ((Bounded 0 2),(Bounded 1 2))
 aliveness : Aliveness

public export
setTemporary : ((Bounded lower upper),t2,t3) -> Integer -> ((Bounded lower upper),t2,t3)
setTemporary (temporary,permanent,base) x = (temporary := x, permanent, base)
public export
setPermanent : (t1,(Bounded lower upper),t3) -> Integer -> (t1,(Bounded lower upper),t3)
setPermanent (temporary,permanent,base) x = (temporary, permanent := x, base)
public export
incrementTemporary : ((Bounded lower upper),t2,t3) -> Integer -> ((Bounded lower upper),t2,t3)
incrementTemporary (temporary,permanent,base) x = (temporary + x, permanent, base)
public export
incrementPermanent : (t1,(Bounded lower upper),t3) -> Integer -> (t1,(Bounded lower upper),t3)
incrementPermanent (temporary,permanent,base) x = (temporary, permanent + x, base)

public export
getTemporary : (Bounded lower upper,t2,t3) -> Bounded lower upper
getTemporary x = fst x
public export
getPermanent : (t1,Bounded lower upper,t3) -> Bounded lower upper
getPermanent x = fst $ snd x
public export
getBase : (t1,t2,Bounded lower upper) -> Bounded lower upper
getBase x = snd $ snd x



public export
record BasicSpell where
 constructor MkBasicSpell
 temporaryId : Nat
 school : Fin 6 {-Spells must have exactly one school-}
 level : Bounded 1 9

public export
data BasicCard = BasicSpellCard BasicSpell | BasicMonsterCard BasicMonster
