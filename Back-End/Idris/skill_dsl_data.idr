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
data DamageEffect = MkDamageEffect Integer
data StatEffect = MkStatEffect Stat Mutator Temporality Integer | MkHpEffect Mutator HpStat Integer | MkEngagementEffect Mutator Integer | ReviveEffect
data ResourceEffect = ThoughtEffect Mutator Integer | SchoolEffect (Fin 6) Mutator Integer
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
applyStatEffect basic (MkEngagementEffect mutator x) = ?hole
applyStatEffect basic ReviveEffect = ?hole


{-SOMEWHERE CARD SKILLS NEED TO KEEP A REFERENCE TO THEIR OWN CARDS-}


data Set = FriendlyBoard | EnemyBoard | FriendlySpawn | EnemySpawn | FriendlyHand | EnemyHand | FriendlyGraveyard | EnemyGraveyard | FriendlyDiscard | EnemyDiscard | Union Set Set

{-dummy stuff for now-}
data StatR = TemporaryAttackR | PermanentAttackR | TemporarySpeedR | PermanentSpeedR | HpR | MaxHpR
mutual
  data RInteger = Constant Integer | Variable StatR String | Plus RInteger RInteger | Minus RInteger RInteger | ThoughtsR Bool | SchoolR Bool (Fin 6) | Cardinality String Set Condition {-no requirement that the condition must reference the bound variable currently-}
  data Condition = Vacuous | RDead String | LT RInteger RInteger | EQ RInteger RInteger | GT RInteger RInteger | LEQ RInteger RInteger | GEQ RInteger RInteger | And Condition Condition | Or Condition Condition


mutual
  data Nonautomatic = TerminatedSkill | Existential (Vect n (String,Set)) Condition Automatic Automatic
  data Automatic = MkAutomatic (List SkillEffect) Nonautomatic | Universal String Condition (List SkillEffect) Nonautomatic {-haven't added all of the code for universal yet...-}
                 {-universal also should take a vector of strings, not just a single string, at some point-}


done : Automatic
done = MkAutomatic [] TerminatedSkill

syntax exists friendly unit [x] success ":" [sel] failure ":" [fail] = Existential [] Vacuous sel fail
syntax exists enemy unit [x] success ":" [sel] failure ":" [fail] = Existential [] Vacuous sel fail


foo : Nonautomatic
foo = exists friendly unit "x" success : done failure : done





