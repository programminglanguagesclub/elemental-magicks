module Card
import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import objects_basic
import skill_dsl_data
import phase
import clientupdates
%access public export
%default total

SkillFactory : Type
SkillFactory = (AutomaticFactory, Nat, Condition)

{-automatic, used, cost-}
Skill : Type
Skill = (Automatic, Bool, Nat, Condition) {-HAVE NOT YET DEALT WITH NAT, CONDITION, AND HOW TO BUILD AND OPERATE THE ACTUAL SKILL FROM THIS....-}

{- condition is presumably in Automatic already.... I don't need this here as well.... -}

instantiateSkill : Nat -> String -> SkillFactory -> Skill
instantiateSkill cardId playerId (automatic,cost,condition) = (instantiateAutomatic automatic cardId playerId,False,cost,condition)

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

record SpellFactory where
 constructor MkSpellFactory
 basic : BasicSpellFactory
 spawnSkill : SkillFactory

record Spell where
 constructor MkSpell
 basic : BasicSpell
 spawnSkill : Skill

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

{-should make id come first in the other instantiate functions as well for consistency-}
instantiateMonster : Nat -> String -> MonsterFactory -> Monster
instantiateMonster cardId playerId monsterFactory =
  MkMonster (instantiateBasicMonster (basic monsterFactory) cardId)
            ((instantiateSkill cardId playerId) <$> (startSkill monsterFactory))
            ((instantiateSkill cardId playerId) <$> (endSkill monsterFactory))
            ((instantiateSkill cardId playerId) <$> (counterSkill monsterFactory))
            ((instantiateSkill cardId playerId) <$> (spawnSkill monsterFactory))
            ((instantiateSkill cardId playerId) <$> (deathSkill monsterFactory))
            ((instantiateSkill cardId playerId) <$> (autoSkill monsterFactory))
            ((instantiateSkill cardId playerId) <$> (actionSkills monsterFactory))
            (instantiateSkill cardId playerId (soulSkill monsterFactory))

instantiateSpell : Nat -> String -> SpellFactory -> Spell
instantiateSpell cardId playerId spellFactory =
  MkSpell (instantiateBasicSpell (basic spellFactory) cardId)
          (instantiateSkill cardId playerId (spawnSkill spellFactory))

data Card = SpellCard Spell | MonsterCard Monster

