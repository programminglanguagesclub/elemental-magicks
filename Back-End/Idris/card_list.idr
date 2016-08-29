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

syntax unit [basic] = MkMonster basic Nothing Nothing Nothing Nothing Nothing Nothing []



{-
syntax monster [basic] startSkill ":" [startSkill] = MkMonster basic startSkill Nothing Nothing Nothing Nothing Nothing []
 

m : Monster
m = monster (mkBasicMonster "mutant pig" 0 0 (TwoSchools 4 5) life: 60 atk: 30 def: 0 spe: 2 rng: 1 lvl: 3 sp: 2)
t : Monster
t = monster (mkBasicMonster "mutant pig" 0 0 (TwoSchools 4 5) life: 60 atk: 30 def: 0 spe: 2 rng: 1 lvl: 3 sp: 2) startSkill : done
-}


composeSkillAdditions : List (Monster -> Monster) -> Monster -> Monster
composeSkillAdditions [] = \m => m
composeSkillAdditions (x::xs) = \m => x $ composeSkillAdditions xs m
{-
syntax skills [skill_list] = composeSkillAdditions skill_list
-}


syntax schools ":" [school_1] [school_2] = TwoSchools school_1 school_2
syntax school  ":" [school_1] = OneSchool school_1
syntax school ":" none = NoSchools 

syntax [unit_name] "<-" skills ":" [skill_list] [_schools] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] lvl ":" [level] sp ":" [soulPoints] =
  MkBasicMonster unit_name 0 0 _schools (mkHp hp)
   ( >> attack << , >> attack << , >> attack << ) ( >> defense << , >> defense << , >> defense << )
   ( >> speed << , >> speed << , >> speed << ) ( >> range << , >> range << , >> range << ) ( >> level << , >> level << , >> level << ) ( >> soulPoints << , >> soulPoints << )
   >> 0 << Alive




{-

Let's ditch permanent ID and just use name.
for now I'll put 0 for all the permanent IDS. Also temporary ID shouldn't be addressed here, but I'll put 0 for now for all of them as well.

syntax mkBasicMonster [name] [permanentId] [temporaryId] [schools] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] lvl ":" [level] sp ":" [soulPoints] =
  MkBasicMonster name permanentId temporaryId schools (mkHp hp)
   ( >> attack << , >> attack << , >> attack << ) ( >> defense << , >> defense << , >> defense << )
   ( >> speed << , >> speed << , >> speed << ) ( >> range << , >> range << , >> range << ) ( >> level << , >> level << , >> level << ) ( >> soulPoints << , >> soulPoints << )
   >> 0 << Alive


schools : 4 5

-}

no_schools : MonsterSchools
no_schools = NoSchools

earth : MonsterSchools
earth = OneSchool 0
fire : MonsterSchools
fire = OneSchool 1
water : MonsterSchools
water = OneSchool 2
air : MonsterSchools
air = OneSchool 3
spirit : MonsterSchools
spirit = OneSchool 4
void : MonsterSchools
void = OneSchool 5

earth_fire : MonsterSchools
earth_fire = TwoSchools 0 1
earth_water : MonsterSchools
earth_water = TwoSchools 0 2
earth_air : MonsterSchools
earth_air = TwoSchools 0 3
earth_spirit : MonsterSchools
earth_spirit = TwoSchools 0 4
earth_void : MonsterSchools
earth_void = TwoSchools 0 5
fire_water : MonsterSchools
fire_water = TwoSchools 1 2
fire_air : MonsterSchools
fire_air = TwoSchools 1 3
fire_spirit : MonsterSchools
fire_spirit = TwoSchools 1 4
fire_void : MonsterSchools
fire_void = TwoSchools 1 5
water_air : MonsterSchools
water_air = TwoSchools 2 3
water_spirit : MonsterSchools
water_spirit = TwoSchools 2 4
water_void : MonsterSchools
water_void = TwoSchools 2 5
air_spirit : MonsterSchools
air_spirit = TwoSchools 3 4
air_void : MonsterSchools
air_void = TwoSchools 3 5
spirit_void : MonsterSchools
spirit_void = TwoSchools 4 5





monsterList : List BasicMonster
monsterList = [

  "mutant pig" <- skills : [] spirit_void life: 60 atk: 30 def: 0 spe: 2 rng: 1 lvl: 3 sp: 2


{-{-monster (mkBasicMonster "mutant pig" 0 0 (TwoSchools 4 5) life: 60 atk: 30 def: 0 spe: 2 rng: 1 lvl: 3 sp: 2)
  startSkill : Nothing
  endSkill : Nothing
  counterSkill : Nothing
  spawnSkill : Nothing
  deathSkill : Nothing
  autoSkill : Nothing
  actionSkills :[],-}
  (startSkill : done) (unit (mkBasicMonster "greater succubus" 1 0 (OneSchool 4) life: 60 atk: 0 def: 0 spe: 1 rng: 3 lvl: 3 sp: 2))
 
-}







]
