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



{-I should be able to use auto or something (or at least syntax extensions?) to avoid having to write Oh everywhere.-}
instantiateBasicMonster : BasicMonsterFactory -> Nat -> BasicMonster
{-

{- temporarily disabled! -}

instantiateBasicMonster basicMonsterFactory cardId =
 MkBasicMonster (name basicMonsterFactory)
                cardId
                (schools basicMonsterFactory)
                (mkHp (extractBounded $ hp basicMonsterFactory))
                (triple $ attack basicMonsterFactory)
                (triple $ defense basicMonsterFactory)
                (extendBounds (speed basicMonsterFactory) Oh Oh , extendBounds (speed basicMonsterFactory) Oh Oh, extractBounds (speed basicMonsterFactory) Oh Oh) 
                (extendBounds (range basicMonsterFactory) Oh Oh , extendBounds (range basicMonsterFactory) Oh Oh, extractBounds (range basicMonsterFactory) Oh Oh)
                (extendLowerBound (level basicMonsterFactory) Oh, extendLowerBound (level basicMonsterFactor) Oh, level basicMonsterFactory)
                (extendLowerBound (soulPoints basicMonsterFactory) Oh, soulPoints basicMonsterFactory)
                >> 0 <<
                Alive   





-}


{-

Can do something like this for BasicMonsterFactory

syntax mkBasicMonster [name] [permanentId] [temporaryId] [schools] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] lvl ":" [level] sp ":" [soulPoints] =
  MkBasicMonster name permanentId temporaryId schools (mkHp hp)
   ( >> attack << , >> attack << , >> attack << ) ( >> defense << , >> defense << , >> defense << )
   ( >> speed << , >> speed << , >> speed << ) ( >> range << , >> range << , >> range << ) ( >> level << , >> level << , >> level << ) ( >> soulPoints << , >> soulPoints << )
   >> 0 << Alive
   -}

setTemporary : ((Bounded lower upper),t2,t3) -> Integer -> ((Bounded lower upper),t2,t3)
setTemporary (temporary,permanent,base) x = (temporary := x, permanent, base)
setPermanent : (t1,(Bounded lower upper),t3) -> Integer -> (t1,(Bounded lower upper),t3)
setPermanent (temporary,permanent,base) x = (temporary, permanent := x, base)
incrementTemporary : ((Bounded lower upper),t2,t3) -> Integer -> ((Bounded lower upper),t2,t3)
incrementTemporary (temporary,permanent,base) x = (temporary + x, permanent, base)
incrementPermanent : (t1,(Bounded lower upper),t3) -> Integer -> (t1,(Bounded lower upper),t3)
incrementPermanent (temporary,permanent,base) x = (temporary, permanent + x, base)

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
 id : Nat
 school : Fin 6 {-Spells must have exactly one school-}
 level : Bounded 1 9

instantiateBasicSpell : BasicSpellFactory -> BasicSpell
instantiateBasicSpell basicSpellFactory = ?hole



data BasicCard = BasicSpellCard BasicSpell | BasicMonsterCard BasicMonster






