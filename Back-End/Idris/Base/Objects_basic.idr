module Base.Objects_basic
import Data.Vect
import Data.So
import Base.Preliminaries
import Base.Bounded
import Base.Hp

%access public export
%default total

-------------------------------------------------------------------------------
data Aliveness = Alive | DeadFresh | DeadStale
-------------------------------------------------------------------------------
{-eventually make multischool required to be unique at the type level-}
data MonsterSchools
 = NoSchools
 | OneSchool (Fin 6)
 | TwoSchools (Fin 6) (Fin 6)

DecEq MonsterSchools where
 NoSchools == NoSchools = True
 NoSchools == (OneSchool x) = False
 NoSchools == (TwoSchools x y) = False
 (OneSchool x1) == (OneSchool x2) = x1 == x2
 (OneSchool x1) == (TwoSchools x2 y2) = False
 (TwoSchools x1 x2) == (TwoSchools x2 y2) = x1 == x2 && y1 == y2
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
 soulPoints : Bounded 1 2

DecEq BasicMonsterFactory where
 (MkBasicMonsterFactory name1 schools1 hp1 attack1 defense1 speed1 range1 level1 soulPoints1) == (MkBasicMonsterFactory name2 schools2 hp2 attack2 defense2 speed2 range2 level2 soulPoints2) = ?hole
-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------
resetHp : Hp -> Hp
resetHp (MkHp (_,base)) = generateHp base
-------------------------------------------------------------------------------
resetAttack : temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) -> temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)
resetAttack (temporary, permanent, base) = (temporary, permanent, base)
-------------------------------------------------------------------------------
resetDefense : temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) -> temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)
resetDefense = resetAttack
-------------------------------------------------------------------------------
resetSpeed : (Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound, Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound, Bounded 1 5) ->
             (Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound, Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound, Bounded 1 5)
resetSpeed (temporary, permanent, base) = (extendBounds base Oh Oh, extendBounds base Oh Oh, base)
-------------------------------------------------------------------------------
resetRange : (Bounded 0 Preliminaries.absoluteUpperBound, Bounded 0 Preliminaries.absoluteUpperBound, Bounded 1 5) ->
             (Bounded 0 Preliminaries.absoluteUpperBound, Bounded 0 Preliminaries.absoluteUpperBound, Bounded 1 5)
resetRange (temporary, permanent, base) = (extendBounds base Oh Oh, extendBounds base Oh Oh, base)
-------------------------------------------------------------------------------
resetLevel : (Bounded 0 9, Bounded 0 9, Bounded 1 9) -> (Bounded 0 9, Bounded 0 9, Bounded 1 9)
resetLevel (temporary, permanent, base) = (extendLowerBound base Oh, extendLowerBound base Oh, base)
-------------------------------------------------------------------------------
resetEngagement : Bounded 0 Preliminaries.absoluteUpperBound -> Bounded 0 Preliminaries.absoluteUpperBound
resetEngagement _ = bind 0
-------------------------------------------------------------------------------
revive : BasicMonster -> BasicMonster
revive basic =
  record {hp $= resetHp, attack $= resetAttack, defense $= resetDefense, speed $= resetSpeed, range $= resetRange, level $= resetLevel, engagement $= resetEngagement, aliveness = Alive} basic
-------------------------------------------------------------------------------
triple : t -> (t,t,t)
triple t = (t,t,t)
-------------------------------------------------------------------------------
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
                (bind 0)
                Alive   
-------------------------------------------------------------------------------
setTemporary : ((Bounded lower upper),t2,t3) -> Integer -> ((Bounded lower upper),t2,t3)
setTemporary (temporary,permanent,base) x = (temporary := x, permanent, base)
-------------------------------------------------------------------------------
setPermanent : (t1,(Bounded lower upper),t3) -> Integer -> (t1,(Bounded lower upper),t3)
setPermanent (temporary,permanent,base) x = (temporary, permanent := x, base)
-------------------------------------------------------------------------------
incrementTemporary : ((Bounded lower upper),t2,t3) -> Integer -> ((Bounded lower upper),t2,t3)
incrementTemporary (temporary,permanent,base) x = (temporary + x, permanent, base)
-------------------------------------------------------------------------------
incrementPermanent : (t1,(Bounded lower upper),t3) -> Integer -> (t1,(Bounded lower upper),t3)
incrementPermanent (temporary,permanent,base) x = (temporary, permanent + x, base)
-------------------------------------------------------------------------------
decrementTemporary : ((Bounded lower upper),t2,t3) -> Integer -> ((Bounded lower upper),t2,t3)
decrementTemporary b v = incrementTemporary b (-v)
-------------------------------------------------------------------------------
decrementPermanent : (t1,(Bounded lower upper),t3) -> Integer -> (t1,(Bounded lower upper),t3)
decrementPermanent b v = incrementPermanent b (-v)
-------------------------------------------------------------------------------
getTemporary : (Bounded lower upper,t2,t3) -> Bounded lower upper
getTemporary x = fst x
-------------------------------------------------------------------------------
getPermanent : (t1,Bounded lower upper,t3) -> Bounded lower upper
getPermanent x = fst $ snd x
-------------------------------------------------------------------------------
getBase : (t1,t2,Bounded lower upper) -> Bounded lower upper
getBase x = snd $ snd x
-------------------------------------------------------------------------------
record BasicSpellFactory where
 constructor MkBasicSpellFactory
 name : String
 school : Fin 6
 level : Bounded 1 9
-------------------------------------------------------------------------------
record BasicSpell where
 constructor MkBasicSpell
 name : String
 id : Nat
 school : Fin 6 {-Spells must have exactly one school-}
 level : Bounded 1 9
-------------------------------------------------------------------------------
instantiateBasicSpell : BasicSpellFactory -> Nat -> BasicSpell
instantiateBasicSpell basicSpellFactory id =
  MkBasicSpell (name basicSpellFactory)
               id
               (school basicSpellFactory)
               (level basicSpellFactory)
-------------------------------------------------------------------------------
data BasicCard = BasicSpellCard BasicSpell | BasicMonsterCard BasicMonster
-------------------------------------------------------------------------------

