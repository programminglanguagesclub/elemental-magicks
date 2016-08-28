module Card_list
import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import hp
import preliminaries
import objects_basic
import skill_dsl_data
import skill_dsl_syntax
import phase
import clientupdates
import player
%access public export
%default total


{-temporary ids really make absolutely no sense here at all.
 that needs to be removed from this...
 the cards in the game need to be this plus a temporary id I guess...
 for now I'm just making all of the temporaryIds 0.
-}

{-I should make the syntax require saying the name of the stat before its value...-}


{-not giving mutant pig a skill quite yet-}

{-
t : BasicMonster
t = mkBasicMonster "mutant pig" 0 0 (TwoSchools 4 5) (60) (30) (0) (2) (1) (3) sp 2
-}



{-

syntax mkBasicMonster [name] [permanentId] [temporaryId] [schools] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] lvl ":" [level] sp ":" [soulPoints] =
  MkBasicMonster name permanentId temporaryId schools (mkHp hp)
   ( >> attack << , >> attack << , >> attack << ) ( >> defense << , >> defense << , >> defense << )
   ( >> speed << , >> speed << , >> speed << ) ( >> range << , >> range << , >> range << ) ( >> level << , >> level << , >> level << ) ( >> soulPoints << , >> soulPoints << )
   >> 0 << Alive




startSkill : Maybe Skill
 endSkill : Maybe Skill
 counterSkill : Maybe Skill
 spawnSkill     : Maybe Skill
 deathSkill      : Maybe Skill
 autoSkill   : Maybe Skill
 actionSkills : List Skill

-}
{-
syntax monster [basicMonster] startSkill ":" [startSkill] endSkill ":" [endSkill] counterSkill ":" [counterSkill] spawnSkill ":" [spawnSkill] deathSkill ":" [deathSkill] autoSkill ":" [autoSkill] actionSkills ":" [actionSkills] = MkMonster basicMonster startSkill autoSkill counterSkill spawnSkill deathSkill autoSkill actionSkills
-}


{-CAN ALSO USE THIS IDEA TO REMOVE ALL OF THE TRAILING DONES FROM SKILLS!-}
syntax startSkill ":" [startSkill] = \x => record {startSkill = Just (startSkill, False, 0)} x
syntax endSkill ":" [endSkill] = \x => record {endSkill = Just (endSkill, False, 0)} x
syntax counterSkill ":" [counterSkill] = \x => record {counterSkill = Just (counterSkill, False, 0)} x
syntax spawnSkill ":" [spawnSkill] = \x => record {spawnSkill = Just (spawnSkill, False, 0)} x
syntax deathSkill ":" [deathSkill] = \x => record {deathSkill = Just (deathSkill, False, 0)} x
syntax autoSkill ":" [autoSkill] = \x => record {autoSkill = Just (autoSkill, False, 0)} x
syntax actionSkills ":" [actionSkills] = \x => record {actionSkills = actionSkills} x {-going to have to deal with cost and used somewhere else here....-}


{-
namespace startSkill
  monster : BasicMonster -> Automatic -> Monster
  monster basicMonster automatic = MkMonster 
-}
{-
syntax monster [basic] = MkMonster basic Nothing Nothing Nothing Nothing Nothing Nothing []
syntax monster [basic] startSkill ":" [startSkill] = MkMonster basic startSkill Nothing Nothing Nothing Nothing Nothing []
 

m : Monster
m = monster (mkBasicMonster "mutant pig" 0 0 (TwoSchools 4 5) life: 60 atk: 30 def: 0 spe: 2 rng: 1 lvl: 3 sp: 2)
t : Monster
t = monster (mkBasicMonster "mutant pig" 0 0 (TwoSchools 4 5) life: 60 atk: 30 def: 0 spe: 2 rng: 1 lvl: 3 sp: 2) startSkill : done
-}





monsterList : List Monster
monsterList = [
 {-monster (mkBasicMonster "mutant pig" 0 0 (TwoSchools 4 5) life: 60 atk: 30 def: 0 spe: 2 rng: 1 lvl: 3 sp: 2)
  startSkill : Nothing
  endSkill : Nothing
  counterSkill : Nothing
  spawnSkill : Nothing
  deathSkill : Nothing
  autoSkill : Nothing
  actionSkills :[],-}
  (startSkill : done) (MkMonster (mkBasicMonster "greater succubus" 1 0 (OneSchool 4) life: 60 atk: 0 def: 0 spe: 1 rng: 3 lvl: 3 sp: 2) Nothing Nothing Nothing Nothing Nothing Nothing [])
 








]
