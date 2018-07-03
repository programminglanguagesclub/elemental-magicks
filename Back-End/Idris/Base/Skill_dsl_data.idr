import Data.Vect
import Data.So
import Base.Bounded
import Base.Hp
import Base.Preliminaries
import Base.Objects_basic
import Base.Clientupdates

%access public export
%default total

-------------------------------------------------------------------------------
data Stat
 = Attack
 | Defense
 | Speed
 | Range
 | Level
-------------------------------------------------------------------------------
implementation Show Stat where
 show Attack = "attack"
 show Defense = "defense"
 show Speed = "speed"
 show Range = "range"
 show Level = "level"
-------------------------------------------------------------------------------
data Mutator
 = Increment
 | Decrement
 | Assign
-------------------------------------------------------------------------------
implementation Show Mutator where
 show Increment = "increment"
 show Decrement = "decrement"
 show Assign = "assign"
-------------------------------------------------------------------------------
data Temporality
 = Temporary
 | Permanent
-------------------------------------------------------------------------------
implementation Show Temporality where
 show Temporary = "temporary"
 show Permanent = "permanent"
-------------------------------------------------------------------------------
data HpStat
 = CurrentHp
 | MaxHp
-------------------------------------------------------------------------------
implementation Show HpStat where
 show CurrentHp = "hp"
 show MaxHp = "max hp"
-------------------------------------------------------------------------------
marshall :
 Temporality ->
 (Bounded lower upper, Bounded lower upper, t3) ->
 String

marshall Temporary (x,_,_) = show $ extractBounded x
marshall Permanent (_,x,_) = show $ extractBounded x
-------------------------------------------------------------------------------
marshallHp :
 HpStat ->
 Hp ->
 String

marshallHp CurrentHp (MkHp((currentHp**(maxHp**prf)),baseHp)) =
 show $ extractBounded currentHp
marshallHp MaxHp (MkHp((currentHp**(maxHp**prf)),baseHp)) =
 show $ extractBounded maxHp
-------------------------------------------------------------------------------
selectMutator :
 Mutator ->
 Temporality ->
 (Bounded lower upper, Bounded lower upper, base) ->
 Integer ->
 (Bounded lower upper, Bounded lower upper, base)

selectMutator Assign Temporary = setTemporary
selectMutator Assign Permanent = setPermanent
selectMutator Decrement Temporary = decrementTemporary
selectMutator Decrement Permanent = decrementPermanent
selectMutator Increment Temporary = incrementTemporary
selectMutator Increment Permanent = incrementPermanent
-------------------------------------------------------------------------------
statTypeLower : Stat -> Integer
statTypeLower Speed = Preliminaries.absoluteLowerBound
statTypeLower _ = 0
-------------------------------------------------------------------------------
statTypeUpper : Stat -> Integer
statTypeUpper Level = 9
statTypeUpper _ = Preliminaries.absoluteUpperBound
-------------------------------------------------------------------------------
statTypeLowerBase : Stat -> Integer
statTypeLowerBase Speed = 1
statTypeLowerBase Range = 1
statTypeLowerBase Level = 1
statTypeLowerBase _ = 0
-------------------------------------------------------------------------------
statTypeUpperBase : Stat -> Integer
statTypeUpperBase Speed = 5
statTypeUpperBase Range = 5
statTypeUpperBase Level = 9
statTypeUpperBase _ = Preliminaries.absoluteUpperBound
-------------------------------------------------------------------------------
basicStatType : Stat -> Type

basicStatType stat =
 let temporary = Bounded (statTypeLower stat) (statTypeUpper stat) in
 let permanent = Bounded (statTypeLower stat) (statTypeUpper stat) in
 let base = Bounded (statTypeLowerBase stat) (statTypeUpperBase stat) in
 (temporary, permanent, base)
-------------------------------------------------------------------------------
basicStat : (s : Stat) -> BasicFieldedMonster -> basicStatType s
basicStat Attack = attack
basicStat Defense = defense
basicStat Speed = speed
basicStat Range = range
basicStat Level = level
-------------------------------------------------------------------------------
basicStatSetter : (s : Stat) -> basicStatType s -> BasicFieldedMonster -> BasicFieldedMonster
basicStatSetter Attack = set_attack
basicStatSetter Defense = set_defense
basicStatSetter Speed = set_speed
basicStatSetter Range = set_range
basicStatSetter Level = set_level
-------------------------------------------------------------------------------
hpTransformType : HpStat -> (Integer -> Integer) -> Hp -> Hp
hpTransformType CurrentHp = transformHp
hpTransformType MaxHp = transformMaxHp
-------------------------------------------------------------------------------
hpTransformMutator : Mutator -> Integer -> Integer -> Integer
hpTransformMutator Increment x h = h + x
hpTransformMutator Decrement x h = h - x
hpTransformMutator Assign x h = x
-------------------------------------------------------------------------------
engagementTransformMutator :
 Mutator ->
 Integer ->
 Bounded 0 Preliminaries.absoluteUpperBound ->
 Bounded 0 Preliminaries.absoluteUpperBound

engagementTransformMutator Increment x h = ?hole ---(+) h x
engagementTransformMutator Decrement x h = ?hole ---(-) h x
engagementTransformMutator Assign x h = ?hole ---x
-------------------------------------------------------------------------------
data Set
 = FriendlyBoard 
 | EnemyBoard 
 | FriendlySpawn 
 | EnemySpawn 
 | FriendlyHand 
 | EnemyHand 
 | FriendlyGraveyard 
 | EnemyGraveyard 
 | FriendlyBanished
 | EnemyBanished
 | Union Set Set
-------------------------------------------------------------------------------
data Side
 = Friendly 
 | Enemy
-------------------------------------------------------------------------------
data RelativeSet
 = RelativeBoard 
 | RelativeSpawn 
 | RelativeHand 
 | RelativeGraveyard 
 | RelativeBanished
-------------------------------------------------------------------------------
getSet : Side -> RelativeSet -> Set
getSet Friendly RelativeBoard = FriendlyBoard
getSet Friendly RelativeSpawn = FriendlySpawn
getSet Friendly RelativeHand = FriendlyHand
getSet Friendly RelativeGraveyard = FriendlyGraveyard
getSet Friendly RelativeBanished = FriendlyBanished
getSet Enemy RelativeBoard = EnemyBoard
getSet Enemy RelativeSpawn = EnemySpawn
getSet Enemy RelativeHand = EnemyHand
getSet Enemy RelativeGraveyard = EnemyGraveyard
getSet Enemy RelativeBanished = EnemyBanished
-------------------------------------------------------------------------------
data StatR
 = TemporaryAttackR 
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
-------------------------------------------------------------------------------
mutual
-------------------------------------------------------------------------------
  data DamageEffect = MkDamageEffect RInteger
-------------------------------------------------------------------------------
  data StatEffect
   = MkStatEffect Stat Mutator Temporality RInteger 
   | MkHpEffect Mutator HpStat RInteger 
   | MkEngagementEffect Mutator RInteger 
   | ReviveEffect
-------------------------------------------------------------------------------
  data ResourceEffect
   = ThoughtEffect Mutator RInteger {- Do I have side information for these? -} 
   | SchoolEffect (Fin 6) Mutator RInteger
   | DecrementLP RInteger
-------------------------------------------------------------------------------
  data PositionEffect
   = SwapPositions (RelativeSet, Integer) (RelativeSet, Integer) 
   | MoveFromTo (RelativeSet, Integer) (RelativeSet, Integer)
-------------------------------------------------------------------------------
  data SkillEffect
   = EvokerSkillEffectStatEffect StatEffect 
   | SkillEffectStatEffect StatEffect String 
   | SkillEffectResourceEffect ResourceEffect 
   | SkillEffectPositionEffect PositionEffect 
   | SkillEffectConditional Condition SkillEffect SkillEffect 
   | SkillEffectRowEffect Side String SkillEffect String
     --does effect to all units in row of unit bound to string;
     --the last string binds the respective units in the row for use in SkillEffect
   | SkillEffectColumnEffect Side String SkillEffect String
     --does effect to all units in column of unit bound to string;
     --the last string binds the respective units in the column for use in SkillEffect
   | SkillEffectBehind Side String SkillEffect String
   | SkillEffectInFront Side String SkillEffect String
   | SkillEffectRightOf Side String SkillEffect String
   | SkillEffectLeftOf Side String SkillEffect String
   | SkillEffectBoardPositions Side (List (Bounded 1 9)) SkillEffect String
{- no requirement that elements be unique yet....; the last string binds the respective units for use in SkillEffect -}
-------------------------------------------------------------------------------
  data RInteger
   = Constant Integer 
   | Evoker StatR 
   | Variable StatR String 
   | Plus RInteger RInteger 
   | Minus RInteger RInteger 
   | Mult RInteger RInteger 
   | ThoughtsR Bool 
   | SchoolR Bool (Fin 6) 
   | Cardinality String Set Condition              
{-no requirement that the condition must reference the bound variable currently-}
  data Condition
   = Vacuous 
   | Never
   | RDead String 
   | NotX String String
{-used for implementing skills that are not allowed to target the evoker-}
   | LT RInteger RInteger 
   | EQ RInteger RInteger 
   | GT RInteger RInteger 
   | LEQ RInteger RInteger 
   | GEQ RInteger RInteger 
   | And Condition Condition 
   | Or Condition Condition 
   | Not Condition
   | Exists String Condition
{-at some level, this shouldn't have string, instead a fresh string (that won't be shadowed) should be given... but for now it has a string built in (same for All) -}
   | All String Condition
-------------------------------------------------------------------------------
addCondition : Condition -> Condition -> Condition
addCondition Vacuous additional = additional
addCondition otherwise additional = And otherwise additional
-------------------------------------------------------------------------------
data FixedStatEffect
 = MkFixedStatEffect Stat Mutator Temporality Integer
 | MkFixedHpEffect Mutator HpStat Integer
 | MkFixedEngagementEffect Mutator Integer
 | FixedReviveEffect
-------------------------------------------------------------------------------
applyFixedStatEffect :
 (Fin 9) ->
 WhichPlayer ->
 BasicFieldedMonster ->
 FixedStatEffect ->
 (BasicFieldedMonster, ClientUpdate)
---------------------------------------
applyFixedStatEffect
 monsterIndex
 whichPlayer
 basic
 (MkFixedStatEffect stat mutator temporality value) =
 
 (basicStatSetter
  stat
  (selectMutator mutator temporality (basicStat stat basic) value)
  basic,
 SetStat
  (replaceAt
    monsterIndex
    (Just ((show mutator) ++ (show temporality) ++ (show stat), show value)) $
    replicate 9 Nothing)
  whichPlayer)
---------------------------------------
applyFixedStatEffect
 monsterIndex
 whichPlayer
 basic
 (MkFixedHpEffect mutator hpStat x) =

 let m = record {hp = hpTransformType hpStat (hpTransformMutator mutator x) $ hp basic} basic in
 (m,
  SetStat
   (replaceAt
     monsterIndex
     (Just $ (show hpStat, marshallHp hpStat $ hp m)) $
     replicate 9 Nothing)
   whichPlayer)
---------------------------------------
applyFixedStatEffect
 monsterIndex
 whichPlayer
 basic
 (MkFixedEngagementEffect mutator x) = ?hole
   -----( {engagement $= (\e => 0 + (engagementTransformMutator mutator x (extractBounded e )))} basic, ?hole {-SetStat "engagement" "0" monsterIndex playerId-})
---------------------------------------
applyFixedStatEffect
 monsterIndex
 whichPlayer
 basic
 FixedReviveEffect =
 
 (revive basic,
 Revive
  (replaceAt
    monsterIndex
    Selected $
    replicate 9 Unselected)
  whichPlayer)
-------------------------------------------------------------------------------
mutual
-------------------------------------------------------------------------------
  data NonautomaticFactory
   = TerminatedSkillFactory 
   | ExistentialFactory (Vect n (String,Set)) Condition AutomaticFactory AutomaticFactory
-------------------------------------------------------------------------------
  data AutomaticFactory
   = MkAutomaticFactory (List SkillEffect) NonautomaticFactory 
   | UniversalFactory (String,Set) Condition (List SkillEffect) NonautomaticFactory
-------------------------------------------------------------------------------
mutual
-------------------------------------------------------------------------------
  data Nonautomatic
   = TerminatedSkill 
   | Existential (Vect n (String,Set)) Condition Automatic Automatic Nat String
-------------------------------------------------------------------------------
  data Automatic
   = MkAutomatic (List SkillEffect) Nonautomatic Nat String 
   | Universal (String,Set) Condition (List SkillEffect) Nonautomatic Nat String
{-haven't added all of the code for universal yet...-}
{-universal also should take a vector of strings, not just a single string, at some point-}
-------------------------------------------------------------------------------
mutual
-------------------------------------------------------------------------------
  instantiateNonautomatic : NonautomaticFactory -> Nat -> String -> Nonautomatic
-------------------------------------------------------------------------------
  instantiateAutomatic : AutomaticFactory -> Nat -> String -> Automatic
-------------------------------------------------------------------------------
  instantiateNonautomatic TerminatedSkillFactory cardId playerId = TerminatedSkill
  instantiateNonautomatic (ExistentialFactory args cond succ fail) cId pId =
   Existential
    args
    cond
    (instantiateAutomatic succ cId pId)
    (instantiateAutomatic fail cId pId)
    cId
    pId
-------------------------------------------------------------------------------
  instantiateAutomatic (MkAutomaticFactory effects next) cardId playerId =
   MkAutomatic
    effects
    (instantiateNonautomatic next cardId playerId)
    cardId
    playerId
  instantiateAutomatic (UniversalFactory arg cond effects next) cId pId =
   Universal arg cond effects (instantiateNonautomatic next cId pId) cId pId
{-I actually can just check to see if the card is still in a place where it can use its skill that is loaded onto the queue precisely at the moment the skill goes to the head!  --- nope: could have been taken off the field and then back on, for instance...-}
-------------------------------------------------------------------------------


