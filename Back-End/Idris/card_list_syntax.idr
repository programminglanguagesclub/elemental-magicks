module Card_list_syntax
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
import card
%access public export
%default total


{-temporary ids really make absolutely no sense here at all.
 that needs to be removed from this...
 the cards in the game need to be this plus a temporary id I guess...
 for now I'm just making all of the temporaryIds 0.
-}

{-I should make the syntax require saying the name of the stat before its value...-}


{-not giving mutant pig a skill quite yet-}

namespace justSkill
  skills : Skill -> Maybe Skill
  skills s = Just s
namespace oneSkill
  skills : Skill -> Skill
  skills = id
namespace manySkills
  skills : Skill -> List Skill
  skills s = [s]
namespace zeroCostJustSkill
  skills : Automatic -> Maybe Skill
  skills a = Just (a,False,0)

{-CAN ALSO USE THIS IDEA TO REMOVE ALL OF THE TRAILING DONES FROM SKILLS!-}
syntax startSkill ":" [startSkill] = \x => record {startSkill = skills startSkill} x
syntax endSkill ":" [endSkill] = \x => record {endSkill = skills endSkill} x
syntax counterSkill ":" [counterSkill] = \x => record {counterSkill = skills counterSkill} x
syntax spawnSkill ":" [spawnSkill] = \x => record {spawnSkill = skills spawnSkill} x
syntax deathSkill ":" [deathSkill] = \x => record {deathSkill = skills deathSkill} x
syntax autoSkill ":" [autoSkill] = \x => record {autoSkill = skills autoSkill} x
syntax actions ":" [actions] = \x => record {actionSkills = actions} x {-going to have to deal with cost and used somewhere else here....-}
syntax action ":" [action] = \x => record {actionSkills = [action]} x
syntax soulSkill ":" [soulSkill] = \x => record {soulSkill = skills soulSkill} x



{-syntax actionSkill ":" [actionSkill] = \x => record {actionSkills = [actionSkill]} x-}


{-
action : Skill -> Monster -> Monster
action s m = record {actionSkills = [s]} m
-}

{-
namespace oneAction
  action : Automatic -> Nat -> List Skill
  action automatic cost = [(automatic, False, cost)]
namespace manyActions
  action : Automatic -> Nat -> Skill
  action automatic cost = (automatic, False, cost)
-}


composeSkillAdditions : List (Monster -> Monster) -> Monster -> Monster
composeSkillAdditions [] = \m => m
composeSkillAdditions (x::xs) = \m => x $ composeSkillAdditions xs m



syntax [unit_name] "<-" [skill_list] [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints] =
  composeSkillAdditions skill_list (MkMonster (MkBasicMonster unit_name 0 0 schools (mkHp hp)
   ( >> attack << , >> attack << , >> attack << ) ( >> defense << , >> defense << , >> defense << )
   ( >> speed << , >> speed << , >> speed << ) ( >> range << , >> range << , >> range << ) ( >> level << , >> level << , >> level << ) ( >> soulPoints << , >> soulPoints << )
   >> 0 << Alive) Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing)




{-

Let's ditch permanent ID and just use name.
for now I'll put 0 for all the permanent IDS. Also temporary ID shouldn't be addressed here, but I'll put 0 for now for all of them as well.

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

{-again, can use namespaces to allow either a list of not (in the case of action skills too maybe-}
{-can make exists a named function and use namespaces to allow starting with automatic or non-automatic and avoid the need for "begin"-}



syntax [val] thoughts "->" [skill] = skills (skill, False, val)
{-currently have to use the plural form even for 1 thought..-}

{- eventually I can make the game automatically add "not dead", etc, based on the effects (so you would have to pick a non-dead unit if you wanted to modify stats other than by reviving)-}



{-
foo : Automatic
foo = all x in friendly board where dead x do [revive x] done
-}


{-
foo : Automatic
foo = all x in friendly board where not dead x do [permanent defense x += 10] done
-}
{-
bar : Stat
bar = defense
-}

{-
permanent : Stat -> String -> Mutator -> Integer -> SkillEffect
-}




{-I THINK INCREASING PERMANENT STATS DOES NOT YET INCREASE THE TEMPORARY ONES.
 OTHER THAN THE CASE OF HIT POINTS, IT SHOULD.
-}
