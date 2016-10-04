module Base.Skill_dsl_data
import Data.Vect
import Data.Fin
import Data.So
import Base.Bounded
import Base.Bounded_then_integer
import Base.Integer_then_bounded
import Base.Hp
import Base.Preliminaries
import Base.Objects_basic
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

data Set = FriendlyBoard 
         | EnemyBoard 
         | FriendlySpawn 
         | EnemySpawn 
         | FriendlyHand 
         | EnemyHand 
         | FriendlyGraveyard 
         | EnemyGraveyard 
         | FriendlyDiscard 
         | EnemyDiscard 
         | Union Set Set

data Side = Friendly 
          | Enemy
data RelativeSet = RelativeBoard 
                 | RelativeSpawn 
                 | RelativeHand 
                 | RelativeGraveyard 
                 | RelativeDiscard {- call discard banished -}
getSet : Side -> RelativeSet -> Set

data StatR = TemporaryAttackR 
           | PermanentAttackR 
           | TemporarySpeedR 
           | PermanentSpeedR 
           | TemporaryDefenseR 
           | PermanentDefenseR 
           | TemporaryRangeR 
           | PermanentRangeR 
           | TemporaryLevelR 
           | PermanentLevelR 
           | HpR 
           | MaxHpR
mutual
  data DamageEffect = MkDamageEffect RInteger
  data StatEffect = MkStatEffect Stat Mutator Temporality RInteger 
                  | MkHpEffect Mutator HpStat RInteger 
                  | MkEngagementEffect Mutator RInteger 
                  | ReviveEffect
  data ResourceEffect = ThoughtEffect Mutator RInteger {- Do I have side information for these? -} 
                      | SchoolEffect (Fin 6) Mutator RInteger
                      | DecrementLP RInteger
  data PositionEffect = SwapPositions (RelativeSet, Integer) (RelativeSet, Integer) 
                      | MoveFromTo (RelativeSet, Integer) (RelativeSet, Integer)
  data SkillEffect = EvokerSkillEffectStatEffect StatEffect 
                   | SkillEffectStatEffect StatEffect String 
                   | SkillEffectResourceEffect ResourceEffect 
                   | SkillEffectPositionEffect PositionEffect 
                   | SkillEffectConditional Condition SkillEffect SkillEffect 
                   | SkillEffectRowEffect Side String SkillEffect String {- does effect to all units in row of unit bound to string; the last string binds the respective units in the row for use in SkillEffect -} 
                   | SkillEffectColumnEffect Side String SkillEffect String {- does effect to all units in column of unit bound to string; the last string binds the respective units in the column for use in SkillEffect -}
                   | SkillEffectBehind Side String SkillEffect String
                   | SkillEffectInFront Side String SkillEffect String
                   | SkillEffectRightOf Side String SkillEffect String
                   | SkillEffectLeftOf Side String SkillEffect String
                   | SkillEffectBoardPositions Side (List (Bounded 1 9)) SkillEffect String {- no requirement that elements be unique yet....; the last string binds the respective units for use in SkillEffect -}
                
  data RInteger = Constant Integer 
                | Evoker StatR 
                | Variable StatR String 
                | Plus RInteger RInteger 
                | Minus RInteger RInteger 
                | Mult RInteger RInteger 
                | ThoughtsR Bool 
                | SchoolR Bool (Fin 6) 
                | Cardinality String Set Condition 
                
                {-no requirement that the condition must reference the bound variable currently-}
  data Condition = Vacuous 
                 | Never
                 | RDead String 
                 | NotX String String {-used for implementing skills that are not allowed to target the evoker-}
                 | LT RInteger RInteger 
                 | EQ RInteger RInteger 
                 | GT RInteger RInteger 
                 | LEQ RInteger RInteger 
                 | GEQ RInteger RInteger 
                 | And Condition Condition 
                 | Or Condition Condition 
                 | Not Condition
                 | Exists String Condition {-at some level, this shouldn't have string, instead a fresh string (that won't be shadowed) should be given... but for now it has a string built in (same for All) -}
                 | All String Condition

addCondition : Condition -> Condition -> Condition
addCondition Vacuous additional = additional
addCondition otherwise additional = And otherwise additional

applyStatEffect : BasicMonster -> StatEffect -> (BasicMonster, (String,String))
applyStatEffect basic (MkStatEffect stat mutator temporality x) =
  {-let m = basicStatSetter stat (selectMutator mutator temporality (basicStat stat basic) x) basic in (m, (getStatTypeName stat, marshall temporality $ basicStat stat m))-} ?hole
applyStatEffect basic (MkHpEffect mutator hpStat x) = {- let m = record {hp = hpTransformType hpStat (hpTransformMutator mutator x) $ hp basic} basic in (m,getHpTypeName hpStat, marshallHp hpStat $ hp m)-} ?hole
applyStatEffect basic (MkEngagementEffect mutator x) = ?hole
applyStatEffect basic ReviveEffect = ?hole

mutual
  data NonautomaticFactory = TerminatedSkillFactory 
                           | ExistentialFactory (Vect n (String,Set)) Condition AutomaticFactory AutomaticFactory
  data AutomaticFactory = MkAutomaticFactory (List SkillEffect) NonautomaticFactory 
                        | UniversalFactory (String,Set) Condition (List SkillEffect) NonautomaticFactory
mutual
  data Nonautomatic = TerminatedSkill Nat String 
                    | Existential (Vect n (String,Set)) Condition Automatic Automatic Nat String
  data Automatic = MkAutomatic (List SkillEffect) Nonautomatic Nat String 
                 | Universal (String,Set) Condition (List SkillEffect) Nonautomatic Nat String {-haven't added all of the code for universal yet...-}
                 {-universal also should take a vector of strings, not just a single string, at some point-}
mutual
  instantiateNonautomatic : NonautomaticFactory -> Nat -> String -> Nonautomatic
  instantiateNonautomatic TerminatedSkillFactory cardId playerId = TerminatedSkill cardId playerId
  instantiateNonautomatic (ExistentialFactory arguments condition success failure) cardId playerId = Existential arguments condition (instantiateAutomatic success cardId playerId) (instantiateAutomatic failure cardId playerId) cardId playerId

  instantiateAutomatic : AutomaticFactory -> Nat -> String -> Automatic
  instantiateAutomatic (MkAutomaticFactory effects next) cardId playerId = MkAutomatic effects (instantiateNonautomatic next cardId playerId) cardId playerId
  instantiateAutomatic (UniversalFactory argument condition effects next) cardId playerId = Universal argument condition effects (instantiateNonautomatic next cardId playerId) cardId playerId

{-I actually can just check to see if the card is still in a place where it can use its skill that is loaded onto the queue precisely at the moment the skill goes to the head!-}
