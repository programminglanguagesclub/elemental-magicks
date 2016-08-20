module Skill_dsl_data
import Data.Vect
import Data.Fin
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import hp
import preliminaries
import objects_basic
%access export
data Stat = Attack | Defense | Speed | Range | Level
data Mutator = Increment | Assign
data Temporality = Temporary | Permanent
data HpStat = CurrentHp | MaxHp
marshall : Temporality -> (Bounded lower upper, Bounded lower upper, t3) -> String
marshall Temporary (x,_,_) = show $ extractBounded x
marshall Permanent (_,x,_) = show $ extractBounded x
marshallHp : HpStat -> Hp -> String
marshallHp CurrentHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = show $ extractBounded currentHp
marshallHp MaxHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = show $ extractBounded maxHp
selectMutator : Mutator -> Temporality -> (Bounded lower upper, Bounded lower upper, base) -> Integer -> (Bounded lower upper, Bounded lower upper, base)
selectMutator Assign Temporary = setTemporary
selectMutator Assign Permanent = setPermanent
selectMutator Increment Temporary = incrementTemporary
selectMutator Increment Permanent = incrementPermanent
data StatEffect = MkStatEffect Stat Mutator Temporality Integer | MkHpEffect Mutator HpStat Integer
data ResourceEffect = ResourceDummy
data PositionEffect = PositionDummy
data SkillEffect = SkillEffectStatEffect StatEffect String | SkillEffectResourceEffect ResourceEffect {- | SkillEffectPositionEffect PositionEffect not sure exactly what arguments..-}
statTypeLower : Stat -> Integer
statTypeLower Speed = Preliminaries.absoluteLowerBound
statTypeLower _ = 0
statTypeUpper : Stat -> Integer
statTypeUpper Level = 9
statTypeUpper _ = Preliminaries.absoluteUpperBound
statTypeLowerBase : Stat -> Integer
statTypeLowerBase Speed = 1
statTypeLowerBase Range = 1
statTypeLowerBase Level = 1
statTypeLowerBase _ = 0
statTypeUpperBase : Stat -> Integer
statTypeUpperBase Speed = 5
statTypeUpperBase Range = 5
statTypeUpperBase Level = 9
statTypeUpperBase _ = Preliminaries.absoluteUpperBound
basicStatType : Stat -> Type
basicStatType stat = (Bounded (statTypeLower stat) (statTypeUpper stat), Bounded (statTypeLower stat) (statTypeUpper stat), Bounded (statTypeLowerBase stat) (statTypeUpperBase stat))
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
hpTransformType : HpStat -> (Integer -> Integer) -> Hp -> Hp
hpTransformType CurrentHp = transformHp
hpTransformType MaxHp = transformMaxHp
hpTransformMutator : Mutator -> Integer -> Integer -> Integer
hpTransformMutator Increment x h = h + x
hpTransformMutator Assign x h = x
getStatTypeName : Stat -> String
getStatTypeName Attack = "attack"
getStatTypeName Defense = "defense"
getStatTypeName Speed = "speed"
getStatTypeName Range = "range"
getStatTypeName Level = "level"
getHpTypeName : HpStat -> String
getHpTypeName CurrentHp = "hp"
getHpTypeName MaxHp = "max hp"
applyStatEffect : BasicMonster -> StatEffect -> (BasicMonster, (String,String))
applyStatEffect basic (MkStatEffect stat mutator temporality x) =
  let m = basicStatSetter stat (selectMutator mutator temporality (basicStat stat basic) x) basic in (m, (getStatTypeName stat, marshall temporality $ basicStat stat m))
applyStatEffect basic (MkHpEffect mutator hpStat x) = let m = record {hp = hpTransformType hpStat (hpTransformMutator mutator x) $ hp basic} basic in (m,getHpTypeName hpStat, marshallHp hpStat $ hp m)

{-dummy stuff for now-}
data StatR = TemporaryAttackR | PermanentAttackR | TemporarySpeedR | PermanentSpeedR
data RInteger = Constant Integer | Variable StatR String | Plus RInteger RInteger | Minus RInteger RInteger
data Condition = LT | EQ | GT | LEQ | GEQ | And Condition Condition | Or Condition Condition


mutual
  data Nonautomatic = TerminatedSkill | Existential (Vect n String) Condition Automatic Automatic
  data Automatic = MkAutomatic (List SkillEffect) (Nonautomatic )
                               







