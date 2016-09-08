module Objects_basic

import Data.Vect
import Data.So
import preliminaries
import bounded
import bounded_then_integer
import integer_then_bounded
import hp

%access public export
%default total

data Aliveness = Alive | DeadFresh | DeadStale

{-eventually make multischool required to be unique at the type level-}
data MonsterSchools = NoSchools
                    | OneSchool (Fin 6)
                    | TwoSchools (Fin 6) (Fin 6)

record BasicMonsterFactory where
 constructor MkBasicMonsterFactory
 name : String
 schools : MonsterSchools
 hp : Bounded 1 Preliminaries.absoluteUpperBound
 attack : Bounded 0 Preliminaries.absoluteUpperBound
 defense : Bounded 0 Preliminaries.absoluteUpperBound
 speed : Bounded 1 5
 range : Bounded 1 5
 level : Bounded 1 9
 soulPoints : Bounded 1 2

record BasicMonster where
 constructor MkBasicMonster
 name : String
 id : Nat
 schools : MonsterSchools
 hp : Hp
 attack : temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)
 defense : temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)
 speed : (Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound, Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound, Bounded 1 5)
 range : (Bounded 0 Preliminaries.absoluteUpperBound, Bounded 0 Preliminaries.absoluteUpperBound, Bounded 1 5)
 level : (Bounded 0 9, Bounded 0 9, Bounded 1 9)
 soulPoints : ((Bounded 0 2),(Bounded 1 2))
 engagement : Bounded 0 Preliminaries.absoluteUpperBound
 aliveness : Aliveness

triple : t -> (t,t,t)
triple t = (t,t,t)

{-
syntax mkBasicMonsterFactory [name] [schools] [hp] [attack] [defense] [speed] [range] [level] [soulPoints] =
  MkBasicMonsterFactory name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)

test_basic_monster_factory : BasicMonsterFactory
test_basic_monster_factory = mkBasicMonsterFactory "Mutant Pig" (TwoSchools 0 1) 10 10 10 2 2 3 2
-}


instantiateBasicMonster : BasicMonsterFactory -> Nat -> BasicMonster

instantiateBasicMonster basicMonsterFactory cardId =
 MkBasicMonster (name basicMonsterFactory)
                cardId
                (schools basicMonsterFactory)
                (generateHp $ hp basicMonsterFactory)
                (triple $ attack basicMonsterFactory)
                (triple $ defense basicMonsterFactory)
                (extendBounds (speed basicMonsterFactory) Oh Oh , extendBounds (speed basicMonsterFactory) Oh Oh, extendBounds (speed basicMonsterFactory) Oh Oh) 
                (extendBounds (range basicMonsterFactory) Oh Oh , extendBounds (range basicMonsterFactory) Oh Oh, extendBounds (range basicMonsterFactory) Oh Oh)
                (extendLowerBound (level basicMonsterFactory) Oh, extendLowerBound (level basicMonsterFactory) Oh, level basicMonsterFactory)
                (extendLowerBound (soulPoints basicMonsterFactory) Oh, soulPoints basicMonsterFactory)
                >> 0 <<
                Alive   

setTemporary : ((Bounded lower upper),t2,t3) -> Integer -> ((Bounded lower upper),t2,t3)
setTemporary (temporary,permanent,base) x = (temporary := x, permanent, base)
setPermanent : (t1,(Bounded lower upper),t3) -> Integer -> (t1,(Bounded lower upper),t3)
setPermanent (temporary,permanent,base) x = (temporary, permanent := x, base)
incrementTemporary : ((Bounded lower upper),t2,t3) -> Integer -> ((Bounded lower upper),t2,t3)
incrementTemporary (temporary,permanent,base) x = (temporary + x, permanent, base)
incrementPermanent : (t1,(Bounded lower upper),t3) -> Integer -> (t1,(Bounded lower upper),t3)
incrementPermanent (temporary,permanent,base) x = (temporary, permanent + x, base)
decrementTemporary : ((Bounded lower upper),t2,t3) -> Integer -> ((Bounded lower upper),t2,t3)
decrementTemporary b v = incrementTemporary b (-v)
decrementPermanent : (t1,(Bounded lower upper),t3) -> Integer -> (t1,(Bounded lower upper),t3)
decrementPermanent b v = incrementPermanent b (-v)

getTemporary : (Bounded lower upper,t2,t3) -> Bounded lower upper
getTemporary x = fst x
getPermanent : (t1,Bounded lower upper,t3) -> Bounded lower upper
getPermanent x = fst $ snd x
getBase : (t1,t2,Bounded lower upper) -> Bounded lower upper
getBase x = snd $ snd x

record BasicSpellFactory where
 constructor MkBasicSpellFactory
 name : String
 school : Fin 6
 level : Bounded 1 9

record BasicSpell where
 constructor MkBasicSpell
 name : String
 id : Nat
 school : Fin 6 {-Spells must have exactly one school-}
 level : Bounded 1 9

instantiateBasicSpell : BasicSpellFactory -> Nat -> BasicSpell
instantiateBasicSpell basicSpellFactory id =
  MkBasicSpell (name basicSpellFactory)
               id
               (school basicSpellFactory)
               (level basicSpellFactory)

data BasicCard = BasicSpellCard BasicSpell | BasicMonsterCard BasicMonster


