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
%access public export
%default total
data Stat = Attack | Defense | Speed | Range | Level
data Mutator = Increment | Decrement | Assign
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
selectMutator Decrement Temporary = decrementTemporary
selectMutator Decrement Permanent = decrementPermanent
selectMutator Increment Temporary = incrementTemporary
selectMutator Increment Permanent = incrementPermanent

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
hpTransformMutator Decrement x h = h - x
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

data Set = FriendlyBoard | EnemyBoard | FriendlySpawn | EnemySpawn | FriendlyHand | EnemyHand | FriendlyGraveyard | EnemyGraveyard | FriendlyDiscard | EnemyDiscard | Union Set Set

data StatR = TemporaryAttackR | PermanentAttackR | TemporarySpeedR | PermanentSpeedR | TemporaryDefenseR | PermanentDefenseR | TemporaryRangeR | PermanentRangeR | TemporaryLevelR | PermanentLevelR | HpR | MaxHpR
mutual
  data DamageEffect = MkDamageEffect RInteger
  data StatEffect = MkStatEffect Stat Mutator Temporality RInteger | MkHpEffect Mutator HpStat RInteger | MkEngagementEffect Mutator RInteger | ReviveEffect
  data ResourceEffect = ThoughtEffect Mutator RInteger | SchoolEffect (Fin 6) Mutator RInteger
  data PositionEffect = PositionDummy
  data SkillEffect = EvokerSkillEffectStatEffect StatEffect | SkillEffectStatEffect StatEffect String | SkillEffectResourceEffect ResourceEffect {- | SkillEffectPositionEffect PositionEffect not sure exactly what arguments..-}
  data RInteger = Constant Integer | Evoker StatR | Variable StatR String | Plus RInteger RInteger | Minus RInteger RInteger | Mult RInteger RInteger | ThoughtsR Bool | SchoolR Bool (Fin 6) | Cardinality String Set Condition
                
                {-no requirement that the condition must reference the bound variable currently-}
  
  
  data Condition = Vacuous | RDead String | LT RInteger RInteger | EQ RInteger RInteger | GT RInteger RInteger | LEQ RInteger RInteger | GEQ RInteger RInteger | And Condition Condition | Or Condition Condition | Not Condition

applyStatEffect : BasicMonster -> StatEffect -> (BasicMonster, (String,String))
applyStatEffect basic (MkStatEffect stat mutator temporality x) =
  {-let m = basicStatSetter stat (selectMutator mutator temporality (basicStat stat basic) x) basic in (m, (getStatTypeName stat, marshall temporality $ basicStat stat m))-} ?hole
applyStatEffect basic (MkHpEffect mutator hpStat x) = {- let m = record {hp = hpTransformType hpStat (hpTransformMutator mutator x) $ hp basic} basic in (m,getHpTypeName hpStat, marshallHp hpStat $ hp m)-} ?hole
applyStatEffect basic (MkEngagementEffect mutator x) = ?hole
applyStatEffect basic ReviveEffect = ?hole

mutual
  data NonautomaticFactory = TerminatedSkillFactory | ExistentialFactory (Vect n (String,Set)) Condition AutomaticFactory AutomaticFactory
  data AutomaticFactory = MkAutomaticFactory (List SkillEffect) NonautomaticFactory | UniversalFactory (String,Set) Condition (List SkillEffect) NonautomaticFactory


{- HERE I have a NAT, which represents the id of the card, but I should have another identifier which identifies the player (probably another Nat). This is because skills can target both the evoker and the players. From a single id for the player with the card, I can recreate the opponent as well-}

mutual
  data Nonautomatic = TerminatedSkill Nat | Existential (Vect n (String,Set)) Condition Automatic Automatic Nat
  data Automatic = MkAutomatic (List SkillEffect) Nonautomatic Nat | Universal (String,Set) Condition (List SkillEffect) Nonautomatic Nat {-haven't added all of the code for universal yet...-}
                 {-universal also should take a vector of strings, not just a single string, at some point-}
mutual
  instantiateNonautomatic : NonautomaticFactory -> Nat -> Nonautomatic
  instantiateNonautomatic TerminatedSkillFactory id = TerminatedSkill id
  instantiateNonautomatic (ExistentialFactory arguments condition success failure) id = Existential arguments condition (instantiateAutomatic success id) (instantiateAutomatic failure id) id

  instantiateAutomatic : AutomaticFactory -> Nat -> Automatic
  instantiateAutomatic (MkAutomaticFactory effects next) id = MkAutomatic effects (instantiateNonautomatic next id) id
  instantiateAutomatic (UniversalFactory argument condition effects next) id = Universal argument condition effects (instantiateNonautomatic next id) id

{-I actually can just check to see if the card is still in a place where it can use its skill that is loaded onto the queue precisely at the moment the skill goes to the head!-}
