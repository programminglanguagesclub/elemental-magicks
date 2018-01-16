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
data CardId
 = MkCardId (Fin 60) --? -- unused for now...
-------------------------------------------------------------------------------
data Cost =
 MkCost RInteger -- The compiler is probably currently expecting a natural number here. I will need to fix this.
-------------------------------------------------------------------------------
data SkillFactory
 = MkSkillFactory AutomaticFactory Cost Condition
-------------------------------------------------------------------------------
data SkillUsedness
 = Unused
 | Used
-- if a skill is put on the head, I need to purge the skill queue for copies of the skill.
-- equivalently, I need to check against whether a skill is used before putting it on the head.
-------------------------------------------------------------------------------
data Skill
 = MkSkill Automatic Cost Condition SkillType -- automatic has cardId
-- important to have condition and cost here as well, not just in automatic,
-- because I do not count a skill as having been triggered
-- if it does not meet its cost or condition.
-------------------------------------------------------------------------------
instantiateSkill :
 Nat ->
 String ->
 SkillFactory ->
 SkillType ->
 Skill

instantiateSkill cardId playerId (MkSkillFactory automaticFactory cost condition) skillType =
 MkSkill (instantiateAutomatic automaticFactory cardId playerId) cost condition skillType
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
 startSkill : Maybe (Skill, SkillUsedness)
 endSkill : Maybe (Skill, SkillUsedness)
 counterSkill : Maybe (Skill, SkillUsedness)
 spawnSkill : Maybe Skill
 deathSkill : Maybe (Skill, SkillUsedness)
 autoSkill : Maybe (Skill, SkillUsedness)
 actionSkills : List Skill
 soulSkill : Skill
-------------------------------------------------------------------------------
initializeSkillUsedness : Maybe Skill -> Maybe (Skill, SkillUsedness) 
initializeSkillUsedness skill = MkPair <$> skill <*> (pure Unused)
-------------------------------------------------------------------------------
getFactoryAccessor : SkillType -> (MonsterFactory -> Maybe SkillFactory)
-------------------------------------------------------------------------------
instantiateSpecificSkill :
 SkillType ->
 Nat ->
 String ->
 MonsterFactory ->
 Maybe (Skill, SkillUsedness)
 
instantiateSpecificSkill skillType cardId playerId monsterFactory =
 let accessor = getFactoryAccessor skillType in
 initializeSkillUsedness $ (instantiateSkill cardId playerId) <$> (accessor monsterFactory) <*> (pure skillType)

-------------------------------------------------------------------------------
instantiateStartSkill :
 Nat ->
 String ->
 MonsterFactory ->
 Maybe (Skill, SkillUsedness)

instantiateStartSkill = instantiateSpecificSkill StartSkill
-------------------------------------------------------------------------------
instantiateEndSkill :
 Nat ->
 String ->
 MonsterFactory ->
 Maybe (Skill, SkillUsedness)

instantiateEndSkill = instantiateSpecificSkill EndSkill
-------------------------------------------------------------------------------
instantiateCounterSkill :
 Nat ->
 String ->
 MonsterFactory ->
 Maybe (Skill, SkillUsedness)

instantiateCounterSkill = instantiateSpecificSkill CounterSkill
-------------------------------------------------------------------------------
instantiateSpawnSkill :
 Nat ->
 String ->
 MonsterFactory ->
 Maybe Skill
-------------------------------------------------------------------------------
instantiateDeathSkill :
 Nat ->
 String ->
 MonsterFactory ->
 Maybe (Skill, SkillUsedness)

instantiateDeathSkill = instantiateSpecificSkill DeathSkill
-------------------------------------------------------------------------------
instantiateAutoSkill :
 Nat ->
 String ->
 MonsterFactory ->
 Maybe (Skill, SkillUsedness)

instantiateAutoSkill = instantiateSpecificSkill AutoSkill
-------------------------------------------------------------------------------
instantiateActionSkills :
 Nat ->
 String ->
 MonsterFactory ->
 List Skill

instantiateActionSkills cardId playerId monsterFactory =
 map (\x => instantiateSkill cardId playerId x ActionSkill) (actionSkills monsterFactory)
-------------------------------------------------------------------------------
instantiateSoulSkill :
 Nat ->
 String ->
 MonsterFactory ->
 Skill

instantiateSoulSkill cardId playerId monsterFactory =
 (instantiateSkill cardId playerId) (soulSkill monsterFactory) SoulSkill
-------------------------------------------------------------------------------
instantiateMonster : Nat -> String -> MonsterFactory -> Monster

instantiateMonster cardId playerId monsterFactory =
 MkMonster
  (instantiateBasicMonster (basic monsterFactory) cardId)
  (instantiateStartSkill cardId playerId monsterFactory)
  (instantiateEndSkill cardId playerId monsterFactory)
  (instantiateCounterSkill cardId playerId monsterFactory)
  (instantiateSpawnSkill cardId playerId monsterFactory)
  (instantiateDeathSkill cardId playerId monsterFactory)
  (instantiateAutoSkill cardId playerId monsterFactory)
  (instantiateActionSkills cardId playerId monsterFactory)
  (instantiateSoulSkill cardId playerId monsterFactory)
------------------------------------------------------------------------------0
instantiateSpell : Nat -> String -> SpellFactory -> Spell

instantiateSpell cardId playerId spellFactory =
 MkSpell
  (instantiateBasicSpell (basic spellFactory) cardId)
  (instantiateSkill cardId playerId (spawnSkill spellFactory) SpawnSkill)
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
 (Monster -> Maybe (Skill, SkillUsedness)) ->
 Monster ->
 Maybe Skill

getCanUseSkill accessor monster with (accessor monster)
 | Nothing = Nothing
 | Just (skill,Used) = Just (skill)
 | Just (_,Unused) = Nothing
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
 (Monster -> Maybe (Skill, SkillUsedness)) ->
 Bool ->
 Monster ->
 Monster

setCanUseSkill accessor value monster with (accessor monster)
 | Nothing = monster
 | Just (skill, usedness) = ?hole -- set appropriately.
-------------------------------------------------------------------------------
-- Want setters for specific skills now.
-------------------------------------------------------------------------------
setCanUseDeathSkill : Bool -> Monster -> Monster
setCanUseDeathSkill = ?hole --setCanUseSkill deathSkill
-------------------------------------------------------------------------------
setCanUseCounterSkill : Bool -> Monster -> Monster
setCanUseCounterSkill = ?hole --setCanUseSkill counterSkill
-------------------------------------------------------------------------------
setCanUseAutoSkill : Bool -> Monster -> Monster
setCanUseAutoSkill = ?hole --setCanUseSkill autoSkill
-------------------------------------------------------------------------------
setCanUseStartSkill : Bool -> Monster -> Monster
setCanUseStartSkill = ?hole --setCanUseSkill startSkill
-------------------------------------------------------------------------------
setCanUseEndSkill : Bool -> Monster -> Monster
setCanUseEndSkill = ?hole --setCanUseSkill endSkill
-------------------------------------------------------------------------------
setCanUseSpawnSkill : Bool -> Monster -> Monster
setCanUseSpawnSkill = ?hole --setCanUseSkill spawnSkill
-------------------------------------------------------------------------------










