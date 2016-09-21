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


{-not giving mutant pig a skill quite yet-}

namespace justSkill
  skills : SkillFactory -> Maybe SkillFactory
  skills s = Just s
namespace oneSkill
  skills : SkillFactory -> SkillFactory
  skills = id
namespace manySkills
  skills : SkillFactory -> List SkillFactory
  skills s = [s]
namespace zeroCostJustSkill
  skills : AutomaticFactory -> Maybe SkillFactory
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

composeSkillAdditions : List (MonsterFactory -> MonsterFactory) -> MonsterFactory -> MonsterFactory
composeSkillAdditions [] = \m => m
composeSkillAdditions (x::xs) = \m => x $ composeSkillAdditions xs m


syntax [unit_name] "<-" [skill_list] [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints] =
  composeSkillAdditions skill_list (MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<)
   (>> attack <<) (>> defense <<)
   (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing Nothing Nothing [] (done,False,0))

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




{-I THINK INCREASING PERMANENT STATS DOES NOT YET INCREASE THE TEMPORARY ONES.
 OTHER THAN THE CASE OF HIT POINTS, IT SHOULD.
-}
