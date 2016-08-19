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




{-


Something like “basicStatType stat = (Bounded (statTypeLower stat) (statTypeUpper stat), Bounded (statTypeLower stat) (statTypeUpper stat), statTypeBase stat)” or so. Then since there’s no case split an application of basicStatType can reduce right away, and that is far enough to unify it with the required type.



-Melvar
                                       -}



{-gosh, speed has the wrong type as well (wrong bounds...) URG!!-}

basicStatType : Stat -> Type
basicStatType Attack = temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) {--> BasicMonster -> BasicMonster-}
basicStatType Defense = temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) {--> BasicMonster -> BasicMonster-}
basicStatType Speed = temporaryPermanentBase (Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound) {--> BasicMonster -> BasicMonster-}
basicStatType Range = temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) {--> BasicMonster -> BasicMonster -}{-range actually needs to be different... base should be 1-5. actual 0-5-}
basicStatType Level = (Bounded 0 9, Bounded 0 9, Bounded 1 9) {--> BasicMonster -> BasicMonster-}

basicStat : (s : Stat) -> BasicMonster -> basicStatType s
basicStat Attack = attack
basicStat Defense = defense
basicStat Speed = speed
basicStat Range = range
basicStat Level = level
{-
basicStatSetterType : Stat -> Type
basicStatSetterType Attack = (basicStatType Attack) -> BasicMonster -> BasicMonster
basicStatSetterType Defense = (basicStatType Defense) -> BasicMonster -> BasicMonster
basicStatSetterType Speed = (basicStatType Speed) -> BasicMonster -> BasicMonster
basicStatSetterType Range = (basicStatType Range) -> BasicMonster -> BasicMonster
basicStatSetterType Level = (basicStatType Level) -> BasicMonster -> BasicMonster
-}
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



applyStatEffect' : BasicMonster -> StatEffect -> String -> (BasicMonster, (String,String))
applyStatEffect' basic (MkStatEffect stat mutator temporality x) name = let m = (basicStatSetter stat) ((selectMutator mutator temporality) ((basicStat stat) basic) x) basic in
                                                                            (m, (name, marshall temporality((basicStat stat)m)))
                                                                          

{-
syntax applyStatEffect'-}

{-
applyStatEffect : BasicMonster -> StatEffect -> (BasicMonster, (String,String))
applyStatEffect basic (MkStatEffect stat mutator temporality x) = let (setter,getter) = (case stat of
                                                                                             Attack => (basicStatSetter Attack, basicStat Attack)
                                                                                             Defense => (basicStatSetter Defense, basicStat Defense)
                                                                                             Speed => (basicStatSetter Speed, basicStat Speed)
                                                                                             Range => (basicStatSetter Range, basicStat Range)
                                                                                             Level => (basicStatSetter Level, basicStat Level) in ?hole


-}
{-
applyStatEffect : BasicMonster -> StatEffect -> (BasicMonster, (String,String)) {-wrong to triple which even changes the base but okay...-}
applyStatEffect basic (MkStatEffect Attack mutator temporality x) = let m = (basicStatSetter Attack) ((selectMutator mutator temporality) ((basicStat Attack) basic) x) basic in
                                                                        (m, ("attack", marshall temporality((basicStat Attack) m))) {-need to know if I want temporary or permanet, etc....-}
applyStatEffect basic (MkStatEffect Defense mutator temporality x) = let m = (basicStatSetter Defense) ((selectMutator mutator temporality) (defense basic) x) basic in
                                                                         (m, ("defense", marshall temporality((basicStat Defense) m)))
applyStatEffect basic (MkStatEffect Speed mutator temporality x) = let m = (basicStatSetter Speed) ((selectMutator mutator temporality) ((basicStat Speed) basic) x) basic in
                                                                       (m, ("speed", marshall temporality((basicStat Speed) m)))
applyStatEffect basic (MkStatEffect Range mutator temporality x) = let m = (basicStatSetter Range) ((selectMutator mutator temporality) ((basicStat Range) basic) x) basic in
                                                                       (m, ("range", marshall temporality((basicStat Range) m)))
applyStatEffect basic (MkStatEffect Level mutator temporality x) = let m = (basicStatSetter Level) ((selectMutator mutator temporality) ((basicStat Level) basic) x) basic in
                                                                       (m, ("level", marshall temporality((basicStat Level) m)))
applyStatEffect basic (MkHpEffect Mutator CurrentHp x) = ?hole
applyStatEffect basic (MkHpEffect Mutator MaxHp x) = ?hole
-}




{-
getStatSetter : Stat -> (Bounded 0 Preliminaries.absoluteUpperBound, Bounded 0 Preliminaries.absoluteUpperBound, t) -> BasicMonster -> BasicMonster
getStatSetter Attack = set_attack
getStatSetter Defense = set_defense
getStatSetter Speed = set_speed
getStatSetter Range = set_range
getStatSetter Level = set_level

-}


baz : BasicMonster -> temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)
baz = attack

blarg : temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) -> BasicMonster -> BasicMonster
blarg = set_attack

quux : BasicMonster -> BasicMonster
quux m = blarg (attack m) m

{-what follows is an experiment that needs to be added above and incorporated...-}


{-

{-gosh, speed has the wrong type as well (wrong bounds...) URG!!-}
bazquux : Stat -> Type
bazquux Attack = temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) {--> BasicMonster -> BasicMonster-}
bazquux Defense = temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) {--> BasicMonster -> BasicMonster-}
bazquux Speed = temporaryPermanentBase (Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound) {--> BasicMonster -> BasicMonster-}
bazquux Range = temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound) {--> BasicMonster -> BasicMonster -}{-range actually needs to be different... base should be 1-5. actual 0-5-}
bazquux Level = (Bounded 0 9, Bounded 0 9, Bounded 1 9) {--> BasicMonster -> BasicMonster-}

foobarbaz : (s : Stat) -> BasicMonster -> bazquux s
foobarbaz Attack m = attack m
foobarbaz Defense m = defense m
foobarbaz Speed m = speed m
foobarbaz Range m = range m
foobarbaz Level m = level m
-}
{-
quux : BasicMonster -> (BasicMonster -> temporaryPermanentBase (Bounded 0 Preliminaries.absoluteUpperBound)) -> 
       -}



