module Base.Card

import Data.Vect
import Base.Preliminaries
import Base.Bounded
import Base.Hp
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
 = MkCardId (Fin 60) -- ? -- unused for now...
-------------------------------------------------------------------------------
data Cost =
 MkCost RInteger -- The compiler is probably currently expecting a natural number here. I will need to fix this.
-------------------------------------------------------------------------------
data SkillUsedness
 = Unused
 | Used
-- if a skill is put on the head, I need to purge the skill queue for copies of the skill. (what does this mean???)
-- equivalently, I need to check against whether a skill is used before putting it on the head.
-------------------------------------------------------------------------------
data Skill
 = MkSkill Automatic Cost Condition
-- important to have condition and cost here as well, not just in automatic,
-- because I do not count a skill as having been triggered
-- if it does not meet its cost or condition.
-------------------------------------------------------------------------------
record Spell where
 constructor MkSpell
 basic : BasicSpell
 spawnSkill : Skill
-------------------------------------------------------------------------------
record FieldedMonster where
 constructor MkFieldedMonster
 basic : BasicFieldedMonster
 muted : Bool

-- This isn't implemented yet, but the first turn
-- that units are on the field, they are muted/silenced unless they were deployed normally.
-- Note that muted/silenced units are also unable to use their action skills.
-- They may only attack, direct attack, move, or rest.
-- This is to prevent infinite loops involving skills that summon and unsummon units.

-- note that normally, all units, including dead ones, have their skills all reset to unused
-- at the beginning of the spawn phase. This way cards that are revived in the spell phase still have use of skills,
-- unless they were summoned in the spell phase, in which case they are muted.

-- infinite loops are prevented in other cases by only allowing units to activate skills once per turn,
-- (oh, question about when attacks are allowed... do I allow them if direct attacks are? If nothing is in range?
-- How about disallow attack unless there is something in range.

 startSkill : Maybe (Skill, SkillUsedness)
 endSkill : Maybe (Skill, SkillUsedness)
 counterSkill : Maybe (Skill, SkillUsedness)
 spawnSkill : Maybe Skill
 deathSkill : Maybe (Skill, SkillUsedness)
 autoSkill : Maybe (Skill, SkillUsedness)
 actionSkills : List Skill
-------------------------------------------------------------------------------


record UnfieldedMonster where -- in the spawn position, hand, graveyard, or banished
 constructor MkUnfieldedMonster
 basic : BasicUnfieldedMonster -- actually not.... only needs base stats....
 startSkill : Maybe Skill
 endSkill : Maybe Skill
 counterSkill : Maybe Skill
 spawnSkill : Maybe Skill
 deathSkill : Maybe Skill
 autoSkill : Maybe Skill
 actionSkills : List Skill
---- This is actually quite similar to monster factory...


record SoulCard where
 constructor MkSoulCard
 name : String
 soulPoints : ((Bounded 0 2),(Bounded 1 2))
-- this is current,max,
-- but I should also include the fact
-- that the current can't be greater than the max
 soulSkill : Skill


-------------------------------------------------------------------------------
data Card
 = SpellCard Spell
 | MonsterCard UnfieldedMonster
-------------------------------------------------------------------------------
getId : Card -> Fin 25
-------------------------------------------------------------------------------
getLiving : Maybe FieldedMonster -> Bool

getLiving Nothing = False
getLiving (Just m) with (aliveness (basic m))
 | Alive = True
 | DeadFresh = False
 | DeadStale = False
-------------------------------------------------------------------------------
getCanUseSkill :
 (FieldedMonster -> Maybe (Skill, SkillUsedness)) ->
 FieldedMonster ->
 Maybe Skill

getCanUseSkill accessor monster with (accessor monster)
 | Nothing = Nothing
 | Just (skill,Used) = Just (skill)
 | Just (_,Unused) = Nothing
-------------------------------------------------------------------------------
getCanUseDeathSkill : FieldedMonster -> Maybe Skill
getCanUseDeathSkill = getCanUseSkill deathSkill
-------------------------------------------------------------------------------
getCanUseCounterSkill : FieldedMonster -> Maybe Skill
getCanUseCounterSkill = getCanUseSkill counterSkill
-------------------------------------------------------------------------------
getCanUseAutoSkill : FieldedMonster -> Maybe Skill
getCanUseAutoSkill = getCanUseSkill autoSkill
-------------------------------------------------------------------------------
getCanUseStartSkill : FieldedMonster -> Maybe Skill
getCanUseStartSkill = getCanUseSkill startSkill
-------------------------------------------------------------------------------
getCanUseEndSkill : FieldedMonster -> Maybe Skill
getCanUseEndSkill = getCanUseSkill endSkill
-------------------------------------------------------------------------------
getCanUseSpawnSkill : FieldedMonster -> Maybe Skill
getCanUseSpawnSkill = spawnSkill
-------------------------------------------------------------------------------
setCanUseSkill :
 (FieldedMonster -> Maybe (Skill, SkillUsedness)) ->
 Bool ->
 FieldedMonster ->
 FieldedMonster

setCanUseSkill accessor value monster with (accessor monster)
 | Nothing = monster
 | Just (skill, usedness) = ?hole -- set appropriately.
-------------------------------------------------------------------------------
setCanUseDeathSkill : Bool -> FieldedMonster -> FieldedMonster
setCanUseDeathSkill = setCanUseSkill deathSkill
-------------------------------------------------------------------------------
setCanUseCounterSkill : Bool -> FieldedMonster -> FieldedMonster
setCanUseCounterSkill = setCanUseSkill counterSkill
-------------------------------------------------------------------------------
setCanUseAutoSkill : Bool -> FieldedMonster -> FieldedMonster
setCanUseAutoSkill = setCanUseSkill autoSkill
-------------------------------------------------------------------------------
setCanUseStartSkill : Bool -> FieldedMonster -> FieldedMonster
setCanUseStartSkill = setCanUseSkill startSkill
-------------------------------------------------------------------------------
setCanUseEndSkill : Bool -> FieldedMonster -> FieldedMonster
setCanUseEndSkill = setCanUseSkill endSkill
-------------------------------------------------------------------------------

interface HasBaseStat a where
 schools : a -> MonsterSchools
 hp : a -> Bounded 1 Preliminaries.absoluteUpperBound
 attack : a -> Bounded 0 Preliminaries.absoluteUpperBound
 defense : a -> Bounded 0 Preliminaries.absoluteUpperBound
 speed : a -> Bounded 1 5
 range : a -> Bounded 1 5
 level : a -> Bounded 1 9


HasBaseStat BasicUnfieldedMonster where
  schools basicUnfieldedMonster = BasicUnfieldedMonster.schools basicUnfieldedMonster
  hp basicUnfieldedMonster = BasicUnfieldedMonster.hp basicUnfieldedMonster
  attack basicUnfieldedMonster = BasicUnfieldedMonster.attack basicUnfieldedMonster
  defense basicUnfieldedMonster = BasicUnfieldedMonster.defense basicUnfieldedMonster
  speed basicUnfieldedMonster = BasicUnfieldedMonster.speed basicUnfieldedMonster
  range basicUnfieldedMonster = BasicUnfieldedMonster.range basicUnfieldedMonster
  level basicUnfieldedMonster = BasicUnfieldedMonster.level basicUnfieldedMonster

HasBaseStat BasicFieldedMonster where
  schools basicFieldedMonster = BasicFieldedMonster.schools basicFieldedMonster
  hp basicFieldedMonster = getBaseHp $ BasicFieldedMonster.hp basicFieldedMonster
  attack basicFieldedMonster = getBase $ BasicFieldedMonster.attack basicFieldedMonster
  defense basicFieldedMonster = getBase $ BasicFieldedMonster.defense basicFieldedMonster
  speed basicFieldedMonster = getBase $ BasicFieldedMonster.speed basicFieldedMonster
  range basicFieldedMonster = getBase $ BasicFieldedMonster.range basicFieldedMonster
  level basicFieldedMonster = getBase $ BasicFieldedMonster.level basicFieldedMonster

HasBaseStat UnfieldedMonster where
  schools unfieldedMonster = BasicUnfieldedMonster.schools $ basic unfieldedMonster
  hp unfieldedMonster = BasicUnfieldedMonster.hp $ basic unfieldedMonster
  attack unfieldedMonster = BasicUnfieldedMonster.attack $ basic unfieldedMonster
  defense unfieldedMonster = BasicUnfieldedMonster.defense $ basic unfieldedMonster
  speed unfieldedMonster = BasicUnfieldedMonster.speed $ basic unfieldedMonster
  range unfieldedMonster = BasicUnfieldedMonster.range $ basic unfieldedMonster
  level unfieldedMonster = BasicUnfieldedMonster.level $ basic unfieldedMonster

HasBaseStat FieldedMonster where
  schools fieldedMonster = BasicFieldedMonster.schools $ basic fieldedMonster
  hp fieldedMonster = getBaseHp $ BasicFieldedMonster.hp $ basic fieldedMonster
  attack fieldedMonster = getBase $ BasicFieldedMonster.attack $ basic fieldedMonster
  defense fieldedMonster = getBase $ BasicFieldedMonster.defense $ basic fieldedMonster
  speed fieldedMonster = getBase $ BasicFieldedMonster.speed $ basic fieldedMonster
  range fieldedMonster = getBase $ BasicFieldedMonster.range $ basic fieldedMonster
  level fieldedMonster = getBase $ BasicFieldedMonster.level $ basic fieldedMonster





