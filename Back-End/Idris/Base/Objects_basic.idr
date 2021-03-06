module Base.Objects_basic
import Data.Vect
import Data.So
import Base.Preliminaries
import Base.Bounded
import Base.Hp
import Pruviloj.Derive.DecEq

%access public export
%default total

%language ElabReflection

-------------------------------------------------------------------------------
data Aliveness = Alive | DeadFresh | DeadStale
-------------------------------------------------------------------------------
{-eventually make multischool required to be unique at the type level-}
data MonsterSchools
 = NoSchools
 | OneSchool (Fin 6)
 | TwoSchools (Fin 6) (Fin 6)

monsterSchoolsDecEq : (x , y : MonsterSchools) -> Dec (x = y)
%runElab deriveDecEq `{monsterSchoolsDecEq}

DecEq MonsterSchools where
 decEq x y = monsterSchoolsDecEq x y
-------------------------------------------------------------------------------
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

basicMonsterFactoryDecEq : (x, y : BasicMonsterFactory) -> Dec (x = y)
%runElab deriveDecEq `{basicMonsterFactoryDecEq}

DecEq BasicMonsterFactory where
 decEq x y = basicMonsterFactoryDecEq x y
-------------------------------------------------------------------------------
record BasicUnfieldedMonster where
 constructor MkBasicUnfieldedMonster
 name : String
 id : Fin 25
 schools : MonsterSchools
 hp : Bounded 1 Preliminaries.absoluteUpperBound
 attack : Bounded 0 Preliminaries.absoluteUpperBound
 defense : Bounded 0 Preliminaries.absoluteUpperBound
 speed : Bounded 1 5
 range : Bounded 1 5
 level : Bounded 1 9

basicUnfieldedMonsterDecEq : (x, y : BasicUnfieldedMonster) -> Dec (x = y)
%runElab deriveDecEq `{basicUnfieldedMonsterDecEq}

DecEq BasicUnfieldedMonster where
 decEq x y = basicUnfieldedMonsterDecEq x y

-------------------------------------------------------------------------------
record BasicFieldedMonster where
 constructor MkBasicFieldedMonster
 name : String
 id : Fin 25
 schools : MonsterSchools
 hp : Hp
 attack : temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)
 defense : temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)
 speed : (Preliminaries.standardBounds, Preliminaries.standardBounds, Bounded 1 5)
 range : (Bounded 0 Preliminaries.absoluteUpperBound, Bounded 0 Preliminaries.absoluteUpperBound, Bounded 1 5)
 level : (Bounded 0 9, Bounded 0 9, Bounded 1 9)
 engagement : Bounded 0 Preliminaries.absoluteUpperBound
 aliveness : Aliveness

basicFieldedMonsterDecEq : (x, y : BasicFieldedMonster) -> Dec (x = y)
%runElab deriveDecEq `{basicFieldedMonsterDecEq}

DecEq BasicFieldedMonster where
 decEq x y = basicFieldedMonsterDecEq x y
-------------------------------------------------------------------------------
resetHp : Hp -> Hp
resetHp (MkHp (_,base)) = generateHp base
-------------------------------------------------------------------------------
resetAttack :
 temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) ->
 temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)

resetAttack (temporary, permanent, base) = (base,base,base)
-------------------------------------------------------------------------------
resetDefense :
 temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) ->
 temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)

resetDefense = resetAttack
-------------------------------------------------------------------------------
resetSpeed :
 (Preliminaries.standardBounds,
  Preliminaries.standardBounds,
  Bounded 1 5) ->
 (Preliminaries.standardBounds,
  Preliminaries.standardBounds,
  Bounded 1 5)

resetSpeed (temporary, permanent, base) =
 (extendBounds base, extendBounds base, base)
-------------------------------------------------------------------------------
resetRange :
 (Bounded 0 Preliminaries.absoluteUpperBound,
  Bounded 0 Preliminaries.absoluteUpperBound,
  Bounded 1 5) ->
 (Bounded 0 Preliminaries.absoluteUpperBound,
  Bounded 0 Preliminaries.absoluteUpperBound,
  Bounded 1 5)

resetRange (temporary, permanent, base) =
 (extendBounds base, extendBounds base, base)
-------------------------------------------------------------------------------
resetLevel :
 (Bounded 0 9, Bounded 0 9, Bounded 1 9) ->
 (Bounded 0 9, Bounded 0 9, Bounded 1 9)

resetLevel (temporary, permanent, base) =
 (extendLowerBound base Oh, extendLowerBound base Oh, base)
-------------------------------------------------------------------------------
resetEngagement :
 Bounded 0 Preliminaries.absoluteUpperBound ->
 Bounded 0 Preliminaries.absoluteUpperBound

resetEngagement _ = bind 0
-------------------------------------------------------------------------------
revive : BasicFieldedMonster -> BasicFieldedMonster
revive basic =
 record {
  hp $= resetHp,
  attack $= resetAttack,
  defense $= resetDefense,
  speed $= resetSpeed,
  range $= resetRange,
  level $= resetLevel,
  engagement $= resetEngagement,
  aliveness = Alive
 } basic
-- could also compose unfield and field...
-------------------------------------------------------------------------------
triple : t -> (t,t,t)
triple t = (t,t,t)
-------------------------------------------------------------------------------
instantiateBasicMonster : BasicMonsterFactory -> Fin 25 -> BasicUnfieldedMonster

instantiateBasicMonster basicMonsterFactory cardId =
 MkBasicUnfieldedMonster
  (name basicMonsterFactory)
  cardId
  (schools basicMonsterFactory)
  (hp basicMonsterFactory)
  (attack basicMonsterFactory)
  (defense basicMonsterFactory)
  (speed basicMonsterFactory) 
  (range basicMonsterFactory)
  (level basicMonsterFactory)
-------------------------------------------------------------------------------
fieldMonster : BasicUnfieldedMonster -> BasicFieldedMonster
fieldMonster basicUnfieldedMonster =
  MkBasicFieldedMonster
  (name basicUnfieldedMonster)
  (id basicUnfieldedMonster)
  (schools basicUnfieldedMonster)
  (generateHp $ hp basicUnfieldedMonster)
  (triple $ attack basicUnfieldedMonster)
  (triple $ defense basicUnfieldedMonster)
  (extendBounds (speed basicUnfieldedMonster) , extendBounds (speed basicUnfieldedMonster), extendBounds (speed basicUnfieldedMonster))
  (extendBounds (range basicUnfieldedMonster), extendBounds (range basicUnfieldedMonster), extendBounds (range basicUnfieldedMonster))
  (extendLowerBound (level basicUnfieldedMonster) Oh, extendLowerBound (level basicUnfieldedMonster) Oh, level basicUnfieldedMonster)
  (bind 0)
  Alive
-------------------------------------------------------------------------------
setTemporary :
 ((Bounded lower upper),t2,t3) ->
 Integer ->
 ((Bounded lower upper),t2,t3)

setTemporary (temporary,permanent,base) x = (temporary := x, permanent, base)
-------------------------------------------------------------------------------
setPermanent :
 (t1,(Bounded lower upper),t3) ->
 Integer ->
 (t1,(Bounded lower upper),t3)

setPermanent (temporary,permanent,base) x = (temporary, permanent := x, base)
-------------------------------------------------------------------------------
incrementTemporary :
 ((Bounded lower upper),t2,t3) ->
 Integer ->
 ((Bounded lower upper),t2,t3)

incrementTemporary (temporary,permanent,base) x = (temporary+x,permanent,base)
-------------------------------------------------------------------------------
incrementPermanent :
 (t1,(Bounded lower upper),t3) ->
 Integer ->
 (t1,(Bounded lower upper),t3)

incrementPermanent (temporary,permanent,base) x = (temporary,permanent+x,base)
-------------------------------------------------------------------------------
decrementTemporary :
 ((Bounded lower upper),t2,t3) ->
 Integer ->
 ((Bounded lower upper),t2,t3)

decrementTemporary b v = incrementTemporary b (-v)
-------------------------------------------------------------------------------
decrementPermanent :
 (t1,(Bounded lower upper),t3) ->
 Integer ->
 (t1,(Bounded lower upper),t3)

decrementPermanent b v = incrementPermanent b (-v)
-------------------------------------------------------------------------------
getTemporary : (Bounded lower upper,t2,t3) -> Bounded lower upper
getTemporary (x,_,_) = x
-------------------------------------------------------------------------------
getPermanent : (t1,Bounded lower upper,t3) -> Bounded lower upper
getPermanent (_,x,_) = x
-------------------------------------------------------------------------------
getBase : (t1,t2,Bounded lower upper) -> Bounded lower upper
getBase (_,_,x) = x
-------------------------------------------------------------------------------
unfieldMonster : BasicFieldedMonster -> BasicUnfieldedMonster
unfieldMonster basicFieldedMonster =
 MkBasicUnfieldedMonster
 (name basicFieldedMonster)
 (id basicFieldedMonster)
 (schools basicFieldedMonster)
 (getBaseHp $ hp basicFieldedMonster) -- hp
 (getBase $ attack basicFieldedMonster)
 (getBase $ defense basicFieldedMonster)
 (getBase $ speed basicFieldedMonster)
 (getBase $ range basicFieldedMonster)
 (getBase $ level basicFieldedMonster)

-------------------------------------------------------------------------------


record BasicSpellFactory where
 constructor MkBasicSpellFactory
 name : String
 school : Fin 6
 level : Bounded 1 9

basicSpellFactoryDecEq : (x, y : BasicSpellFactory) -> Dec (x = y)
%runElab deriveDecEq `{basicSpellFactoryDecEq}

DecEq BasicSpellFactory where
 decEq x y = basicSpellFactoryDecEq x y
-------------------------------------------------------------------------------
record BasicSpell where
 constructor MkBasicSpell
 name : String
 id : Fin 25
 school : Fin 6 {-Spells must have exactly one school-}
 level : Bounded 1 9

basicSpellDecEq : (x, y : BasicSpell) -> Dec (x = y)
%runElab deriveDecEq `{basicSpellDecEq}

DecEq BasicSpell where
 decEq x y = basicSpellDecEq x y
-------------------------------------------------------------------------------
instantiateBasicSpell : BasicSpellFactory -> Fin 25 -> BasicSpell
instantiateBasicSpell basicSpellFactory id =
  MkBasicSpell
   (name basicSpellFactory)
   id
   (school basicSpellFactory)
   (level basicSpellFactory)
-------------------------------------------------------------------------------
data BasicCard = BasicSpellCard BasicSpell | BasicMonsterCard BasicUnfieldedMonster
-------------------------------------------------------------------------------

