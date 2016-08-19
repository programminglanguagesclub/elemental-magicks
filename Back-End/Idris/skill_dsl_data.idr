module Skill_dsl_data

import Data.Vect
import Data.Fin
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import objects_basic



triple : t -> (t,t,t)
triple x = (x,x,x)

foo : Bounded 0 1000
foo = >> 30 <<


data Stat = Attack
          | Defense
          | Speed
          | Range
          | Level

data Mutator = Increment
             | Assign

data Temporality = Temporary
                 | Permanent
data HpStat = CurrentHp
            | MaxHp

marshall : Temporality -> (Bounded lower upper, Bounded lower upper, t3) -> String
marshall Temporary (x,_,_) = show $ extractBounded x
marshall Permanent (_,x,_) = show $ extractBounded x


selectMutator : Mutator -> Temporality -> (Bounded lower upper, Bounded lower upper, base) -> Integer -> (Bounded lower upper, Bounded lower upper, base)
selectMutator Assign Temporary = setTemporary
selectMutator Assign Permanent = setPermanent
selectMutator Increment Temporary = incrementTemporary
selectMutator Increment Permanent = incrementPermanent


data StatEffect = MkStatEffect Stat Mutator Temporality Integer
                | MkHpEffect Mutator HpStat Integer
data ResourceEffect = ResourceDummy
data PositionEffect = PositionDummy

data SkillEffect = SkillEffectStatEffect StatEffect
                 | SkillEffectResourceEffect ResourceEffect
                 | SkillEffectPositionEffect PositionEffect

statTypeLower : Stat -> Integer
statTypeLower Attack = 0
statTypeLower Defense = 0
statTypeLower Speed = Preliminaries.absoluteLowerBound
statTypeLower Range = 0
statTypeLower Level = 0

statTypeUpper : Stat -> Integer
statTypeUpper Attack = Preliminaries.absoluteUpperBound
statTypeUpper Defense = Preliminaries.absoluteUpperBound
statTypeUpper Speed = Preliminaries.absoluteUpperBound
statTypeUpper Range = Preliminaries.absoluteUpperBound
statTypeUpper Level = 9

statTypeLowerBase : Stat -> Integer
statTypeLowerBase Speed = Preliminaries.absoluteLowerBound
statTypeLowerBase Level = 1
statTypeLowerBase _ = 0

statTypeUpperBase : Stat -> Integer
statTypeUpperBase Level = 9
statTypeUpperBase _ = Preliminaries.absoluteUpperBound



basicStatType : Stat -> Type
basicStatType stat = (Bounded (statTypeLower stat) (statTypeUpper stat), Bounded (statTypeLower stat) (statTypeUpper stat), Bounded (statTypeLowerBase stat) (statTypeUpperBase stat))


{-gosh, speed has the wrong type as well (wrong bounds...) URG!!-}
{-
basicStatType : Stat -> Type
basicStatType Attack = temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) {--> BasicMonster -> BasicMonster-}
basicStatType Defense = temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) {--> BasicMonster -> BasicMonster-}
basicStatType Speed = temporaryPermanentBase (Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound) {--> BasicMonster -> BasicMonster-}
basicStatType Range = temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) {--> BasicMonster -> BasicMonster -}{-range actually needs to be different... base should be 1-5. actual 0-5-}
basicStatType Level = (Bounded 0 9, Bounded 0 9, Bounded 1 9) {--> BasicMonster -> BasicMonster-}
-}




basicStat : (s : Stat) -> BasicMonster -> basicStatType s
basicStat Attack = attack
basicStat Defense = defense
basicStat Speed = speed
basicStat Range = range
basicStat Level = level

basicStatSetter : (s : Stat) -> basicStatType s -> BasicMonster -> BasicMonster
basicStatSetter Attack = set_attack
basicStatSetter Defense = set_defense
basicStatSetter Speed = set_speed
basicStatSetter Range = set_range
basicStatSetter Level = set_level


{-here (String,String) refers to the name of the stat followed by the marshalled value of the stat-}


{-Here i'm using attack basic, defense basic, etc, just for the type in the set case.. The actual value gets ignored... I may want to allow for a way for integers to 
be marshalled into arbitrary bounded types....-}


{-the final value of x in the string,string thing is wrong... We need to know the value at the end of this...-}



applyStatEffect : BasicMonster -> StatEffect -> String -> (BasicMonster, (String,String))
applyStatEffect basic (MkStatEffect stat mutator temporality x) name = let m = (basicStatSetter stat) ((selectMutator mutator temporality) ((basicStat stat) basic) x) basic in
                                                                            (m, (name, marshall temporality((basicStat stat)m)))
applyStatEffect basic (MkHpEffect Mutator CurrentHp x) name = ?hole
applyStatEffect basic (MkHpEffect Mutator MaxHp x) name = ?hole


{-what follows is an experiment that needs to be added above and incorporated...-}

