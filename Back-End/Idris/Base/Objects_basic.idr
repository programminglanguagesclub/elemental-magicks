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


noSchoolNotOneSchool : NoSchools = OneSchool x -> Void
noSchoolNotOneSchool Refl impossible

noSchoolNotTwoSchools : NoSchools = TwoSchools x y -> Void
noSchoolNotTwoSchools Refl impossible


{-

                (n = m) -> Void (Type of prfNo)
                        and
                                        (weaken n = weaken m) -> Void (Expected type)



        Type mismatch between
                        OneSchool (FS n) = OneSchool (FS m) (Type of liftedxyEqual)
                                and
                                                OneSchool (weaken n) = OneSchool (weaken m) (Expected type)
                                                        


                                        -}

foobar : (x : Fin n) -> (y : Fin n) -> (FS x = FS y -> Void) -> x = y -> Void
foobar x y prfSuccNotEqual prfEqual = prfSuccNotEqual $ cong prfEqual

blahblarg : (x : Fin n) -> (y : Fin n) -> FS x = FS y -> x = y
blahblarg x _ Refl = Refl

barfoo : (x : Fin n) -> (y : Fin n) -> (x = y -> Void) -> FS x = FS y -> Void
barfoo x y prfDistinct prfEqualSucc = void $ prfDistinct $ blahblarg x y prfEqualSucc

bazbaz : OneSchool (FS n) = OneSchool (FS m) -> OneSchool (weaken n) = OneSchool (weaken m)
bazbaz Refl = Refl
{-
ughugh : (n : Fin a) -> (m : Fin a) -> n = m -> weaken n = weaken m
ughugh FZ FZ _ = Refl
ughugh FZ (FS k) Refl impossible
ughugh (FS k) FZ Refl impossible
ughugh (FS k) (FS l) prfEqual with (decEq k l)
  | Yes prf = ?hole
  | No prf = ?hole --void $ prf prfEqual

-}


uuu : {n : Fin a} -> {m : Fin a} -> weaken n = weaken m -> n = m
--uuu Refl = ?hole --Refl

barblarg : {n : Fin a}  -> {m : Fin a} -> (n = m -> Void) -> weaken n = weaken m -> Void
barblarg prfDistinct prfEqual = void $ prfDistinct $ uuu prfEqual
{-
barblarg {n=FZ} {m=FZ} prfDistinct prfEqual = prfDistinct Refl
barblarg {n=FS k} {m=FZ} prfDistinct prfEqual with (decEq (weaken (FS k)) (weaken FZ))
 | Yes prfYes = ?hole
 | No prfNo = ?hole
barblarg {n=FZ} {m=FS k} prfDistinct prfEqual = ?hole
barblarg {n=FS a} {m=FS b} prfDistinct prfEqual = ?hole
-}

liftOneSchoolInequality : (x : Fin 6) -> (y : Fin 6) -> (x = y -> Void) -> OneSchool x = OneSchool y -> Void
liftOneSchoolInequality FZ FZ xyDistinct liftedxyEqual = xyDistinct Refl
liftOneSchoolInequality FZ (FS k) xyDistict Refl impossible
liftOneSchoolInequality (FS k) FZ xyDistict Refl impossible
liftOneSchoolInequality (FS n) (FS m) xyDistinct liftedxyEqual with (decEq n m)
  | Yes prfYes = xyDistinct $ cong prfYes
  | No prfNo = liftOneSchoolInequality (weaken n) (weaken m) (barblarg prfNo) (bazbaz liftedxyEqual)



monsterSchoolsDecEq : (x , y : MonsterSchools) -> Dec (x = y)
%runElab deriveDecEq `{monsterSchoolsDecEq}

{-
DecEq MonsterSchools where
 decEq NoSchools NoSchools = Yes Refl
 decEq NoSchools (OneSchool x) = No noSchoolNotOneSchool
 decEq NoSchools (TwoSchools x y) = No noSchoolNotTwoSchools
 decEq (OneSchool x1) (OneSchool x2) =
  case decEq x1 x2 of
    Yes prf => rewrite prf in Yes Refl
    No contra => No $ liftOneSchoolInequality x1 x2 contra
 decEq (OneSchool x1) (TwoSchools x2 y2) = No ?hole
 decEq (TwoSchools x1 y1) (TwoSchools x2 y2) =
  case (decEq x1 x2, decEq y1 y2) of
    (Yes prf1, Yes prf2) => rewrite prf1 in rewrite prf2 in Yes Refl
    (_,No contra) => ?hole
    (No contra,_) => ?hole
    -}
   
   
   --x1 == x2 && y1 == y2
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
 decEq (MkBasicMonsterFactory name1 schools1 hp1 attack1 defense1 speed1 range1 level1 soulPoints1) (MkBasicMonsterFactory name2 schools2 hp2 attack2 defense2 speed2 range2 level2 soulPoints2) = ?hole
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

