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



{-here (String,String) refers to the name of the stat followed by the marshalled value of the stat-}


{-Here i'm using attack basic, defense basic, etc, just for the type in the set case.. The actual value gets ignored... I may want to allow for a way for integers to 
be marshalled into arbitrary bounded types....-}


{-the final value of x in the string,string thing is wrong... We need to know the value at the end of this...-}

applyStatEffect : BasicMonster -> StatEffect -> (BasicMonster, (String,String)) {-wrong to triple which even changes the base but okay...-}
applyStatEffect basic (MkStatEffect Attack mutator temporality x) = let m = record {attack = (selectMutator mutator temporality) (attack basic) x} basic in
                                                                        (m, ("attack", marshall temporality(attack m))) {-need to know if I want temporary or permanet, etc....-}
applyStatEffect basic (MkStatEffect Defense mutator temporality x) = let m = record {defense = (selectMutator mutator temporality) (defense basic) x} basic in
                                                                         (m, ("defense", marshall temporality(defense m)))
applyStatEffect basic (MkStatEffect Speed mutator temporality x) = let m = record {speed = (selectMutator mutator temporality) (speed basic) x} basic in
                                                                       (m, ("speed", marshall temporality(speed m)))
applyStatEffect basic (MkStatEffect Range mutator temporality x) = let m = record {range = (selectMutator mutator temporality) (range basic) x} basic in
                                                                       (m, ("range", marshall temporality(range m)))
applyStatEffect basic (MkStatEffect Level mutator temporality x) = let m = record {level = (selectMutator mutator temporality) (level basic) x} basic in
                                                                       (m, ("level", marshall temporality(level m)))
applyStatEffect basic (MkHpEffect Mutator CurrentHp x) = ?hole
applyStatEffect basic (MkHpEffect Mutator MaxHp x) = ?hole







