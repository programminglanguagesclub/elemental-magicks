module Base.Card

import Data.Vect
import Base.Objects_basic
import Base.Skill_dsl_data

%access public export
%default total

-------------------------------------------------------------------------------
data SkillType
 = StartSkill
 | EndSkill
 | CounterSkill
 | SpawnSkill
 | DeathSkill
 | AutoSkill
 | ActionSkill
 | SoulSkill

-- only allow certain types of skills to be executed once per turn
-- some types of skills are automatically removed from the skill queue depending on how cards are moved to different regions of play.
-------------------------------------------------------------------------------
SkillFactory : Type
SkillFactory = (AutomaticFactory, Nat, Condition) --automatic, used, cost
-------------------------------------------------------------------------------
Skill : Type
Skill = (Automatic, Bool, Nat, Condition)
{-HAVE NOT YET DEALT WITH NAT, CONDITION,
AND HOW TO BUILD AND OPERATE THE ACTUAL SKILL FROM THIS....-}

{- condition is presumably in Automatic already....
I don't need this here as well.... -}
-------------------------------------------------------------------------------
instantiateSkill :
 Nat ->
 String ->
 SkillFactory ->
 Skill

instantiateSkill cardId playerId (automatic,cost,condition) =
 (instantiateAutomatic automatic cardId playerId,False,cost,condition)
-------------------------------------------------------------------------------
record MonsterFactory where
 constructor MkMonsterFactory
 basic : BasicMonsterFactory
 startSkill : Maybe SkillFactory
 endSkill : Maybe SkillFactory
 counterSkill : Maybe SkillFactory
 spawnSkill : Maybe SkillFactory
 deathSkill : Maybe SkillFactory
 autoSkill : Maybe SkillFactory
 actionSkills : List SkillFactory
 soulSkill : SkillFactory
-------------------------------------------------------------------------------
record SpellFactory where
 constructor MkSpellFactory
 basic : BasicSpellFactory
 spawnSkill : SkillFactory
-------------------------------------------------------------------------------
record Spell where
 constructor MkSpell
 basic : BasicSpell
 spawnSkill : Skill
-------------------------------------------------------------------------------
record Monster where
 constructor MkMonster
 basic : BasicMonster
 startSkill : Maybe Skill
 endSkill : Maybe Skill
 counterSkill : Maybe Skill
 spawnSkill : Maybe Skill
 deathSkill : Maybe Skill
 autoSkill : Maybe Skill
 actionSkills : List Skill
 soulSkill : Skill
-------------------------------------------------------------------------------
instantiateMonster : Nat -> String -> MonsterFactory -> Monster

instantiateMonster cardId playerId monsterFactory =
 MkMonster
  (instantiateBasicMonster (basic monsterFactory) cardId)
  ((instantiateSkill cardId playerId) <$> (startSkill monsterFactory))
  ((instantiateSkill cardId playerId) <$> (endSkill monsterFactory))
  ((instantiateSkill cardId playerId) <$> (counterSkill monsterFactory))
  ((instantiateSkill cardId playerId) <$> (spawnSkill monsterFactory))
  ((instantiateSkill cardId playerId) <$> (deathSkill monsterFactory))
  ((instantiateSkill cardId playerId) <$> (autoSkill monsterFactory))
  ((instantiateSkill cardId playerId) <$> (actionSkills monsterFactory))
  (instantiateSkill cardId playerId (soulSkill monsterFactory))
------------------------------------------------------------------------------0
instantiateSpell : Nat -> String -> SpellFactory -> Spell

instantiateSpell cardId playerId spellFactory =
 MkSpell
  (instantiateBasicSpell (basic spellFactory) cardId)
  (instantiateSkill cardId playerId (spawnSkill spellFactory))
-------------------------------------------------------------------------------
data CardFactory
 = SpellCardFactory SpellFactory
 | MonsterCardFactory MonsterFactory
-------------------------------------------------------------------------------
data Card
 = SpellCard Spell
 | MonsterCard Monster
-------------------------------------------------------------------------------
getLiving : Maybe Monster -> Bool

getLiving Nothing = False
getLiving (Just m) with (aliveness (basic m))
 | Alive = True
 | DeadFresh = False
 | DeadStale = False
-------------------------------------------------------------------------------
getCanUseSkill :
 (Monster -> Maybe Skill) ->
 Monster ->
 Maybe Skill

getCanUseSkill accessor monster with (accessor monster)
 | Nothing = Nothing
 | Just (automatic, True, cost, condition) = Just (automatic, True, cost, condition)
 | Just (_,False,_,_) = Nothing




--(Automatic, Bool, Nat, Condition)
-- the used flag is not in a good place. 

-------------------------------------------------------------------------------
getCanUseDeathSkill : Monster -> Maybe Skill
getCanUseDeathSkill = getCanUseSkill deathSkill
-------------------------------------------------------------------------------
getCanUseCounterSkill : Monster -> Maybe Skill
getCanUseCounterSkill = getCanUseSkill counterSkill
-------------------------------------------------------------------------------
getCanUseAutoSkill : Monster -> Maybe Skill
getCanUseAutoSkill = getCanUseSkill autoSkill
-------------------------------------------------------------------------------
getCanUseStartSkill : Monster -> Maybe Skill
getCanUseStartSkill = getCanUseSkill startSkill
-------------------------------------------------------------------------------
getCanUseEndSkill : Monster -> Maybe Skill
getCanUseEndSkill = getCanUseSkill endSkill
-------------------------------------------------------------------------------
getCanUseSpawnSkill : Monster -> Maybe Skill
getCanUseSpawnSkill = getCanUseSkill spawnSkill
-------------------------------------------------------------------------------
setCanUseSkill :
 (Monster -> Maybe Skill) ->
 Bool ->
 Monster ->
 Monster

setCanUseSkill accessor value monster with (accessor monster)
 | Nothing = monster
 | Just (automatic, _, cost, condition) = ?hole -- set appropriately.
-------------------------------------------------------------------------------
-- Want setters for specific skills now.
-------------------------------------------------------------------------------
setCanUseDeathSkill : Bool -> Monster -> Monster
setCanUseDeathSkill = setCanUseSkill deathSkill
-------------------------------------------------------------------------------
setCanUseCounterSkill : Bool -> Monster -> Monster
setCanUseCounterSkill = setCanUseSkill counterSkill
-------------------------------------------------------------------------------
setCanUseAutoSkill : Bool -> Monster -> Monster
setCanUseAutoSkill = setCanUseSkill autoSkill
-------------------------------------------------------------------------------
setCanUseStartSkill : Bool -> Monster -> Monster
setCanUseStartSkill = setCanUseSkill startSkill
-------------------------------------------------------------------------------
setCanUseEndSkill : Bool -> Monster -> Monster
setCanUseEndSkill = setCanUseSkill endSkill
-------------------------------------------------------------------------------
setCanUseSpawnSkill : Bool -> Monster -> Monster
setCanUseSpawnSkill = setCanUseSkill spawnSkill
-------------------------------------------------------------------------------










