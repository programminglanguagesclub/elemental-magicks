module Card
import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import objects_basic
import skill_dsl_data
import skill_dsl
import phase
import clientupdates
%access public export
%default total





SkillFactory : Type
SkillFactory = (AutomaticFactory, Bool, Nat)


{-automatic, used, cost-}
Skill : Type
Skill = (Automatic, Bool, Nat)

instantiateSkill : Nat -> String -> SkillFactory -> Skill
instantiateSkill cardId playerId (automatic,bool,cost) = (instantiateAutomatic automatic cardId playerId,bool,cost)


{-THIS IS WHERE I CAN REPRESENT EVOKER!
I need to distinguish between abstract skills and skills that have been bound to a particular card.

That is, I want to be able to write skills for cards, and then when I create a card, have the other fields filled.
-}

{- does a skill exist, and has it been used? This way I can also easily make it so that spell cards don't disappear as soon as they are used and monsters don't become engaged as they use their action skills-}

{- used : Bool, cost : Nat -}




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

