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

instantiateSkill : Nat -> SkillFactory -> Skill
instantiateSkill id (automatic,bool,cost) = (instantiateAutomatic automatic id,bool,cost)


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
instantiateMonster : Nat -> MonsterFactory -> Monster
instantiateMonster id monsterFactory =
  MkMonster (instantiateBasicMonster (basic monsterFactory) id)
            ((instantiateSkill id) <$> (startSkill monsterFactory))
            ((instantiateSkill id) <$> (endSkill monsterFactory))
            ((instantiateSkill id) <$> (counterSkill monsterFactory))
            ((instantiateSkill id) <$> (spawnSkill monsterFactory))
            ((instantiateSkill id) <$> (deathSkill monsterFactory))
            ((instantiateSkill id) <$> (autoSkill monsterFactory))
            ((instantiateSkill id) <$> (actionSkills monsterFactory))
            (instantiateSkill id (soulSkill monsterFactory))

instantiateSpell : Nat -> SpellFactory -> Spell
instantiateSpell id spellFactory =
  MkSpell (instantiateBasicSpell (basic spellFactory) id)
          (instantiateSkill id (spawnSkill spellFactory))


data Card = SpellCard Spell | MonsterCard Monster

