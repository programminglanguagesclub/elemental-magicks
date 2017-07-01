module Base.Card_list_syntax
import Data.Fin
import Base.Bounded
import Base.Objects_basic
import Base.Skill_dsl_data
import Base.Card
%access public export
%default total
{-






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
  skills a = Just (a,0,Vacuous)

{-
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
   (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing Nothing Nothing [] (done,0, Vacuous))

-}

{- this is a huge code smell, but what can be done? -}



data SkillTypes = StartSkill SkillFactory
                | EndSkill SkillFactory
                | CounterSkill SkillFactory
                | SpawnSkill SkillFactory
                | DeathSkill SkillFactory
                | AutoSkill SkillFactory
                | ActionSkills (List SkillFactory)
                | SoulSkill SkillFactory

SkillBundle : Type
SkillBundle = (Maybe SkillFactory, Maybe SkillFactory, Maybe SkillFactory, Maybe SkillFactory, Maybe SkillFactory, Maybe SkillFactory, List SkillFactory, SkillFactory)

{-
 startSkill : Maybe SkillFactory
 endSkill : Maybe SkillFactory
 counterSkill : Maybe SkillFactory
 spawnSkill : Maybe SkillFactory
 deathSkill : Maybe SkillFactory
 autoSkill : Maybe SkillFactory
 actionSkills : List SkillFactory
 soulSkill : SkillFactory
-}

generateMonsterFactory : Nat {- for now -} -> SkillBundle -> MonsterFactory
generateMonsterFactory = ?hole
{-
syntax [unit_name] "<-" [stats] ";" [skills] = generateMonsterFactory stats skills

syntax [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints] = {- statBundle, etc -} 302
syntax soul ":" [soulSkill] = the SkillBundle ?hole


foo : MonsterFactory
foo = "test card" <- NoSchools lvl : 3 life : 50 atk : 20 def : 0 spe : 3 rng : 2 sp : 2 ;
                     soul : (done,False,0,Vacuous)

-}


















{-
syntax [hh] "[[" = the (List Nat) [hh]
syntax "[" [gg] "," [numbers] = gg :: numbers

jj : List Nat
jj = [ 3 , 1 [[
     -}


{-
syntax soul ":" [soulSkill] = 
  -}





{-
syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        start ":" [startSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        start ":" [startSkill] end ":" [endSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing Nothing Nothing [] soulSkill 

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        start ":" [startSkill] counter ":" [counterSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        start ":" [startSkill] spawn ":" [spawnSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        start ":" [startSkill] death ":" [deathSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        start ":" [startSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        start ":" [startSkill] "action" ":" [actionSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        start ":" [startSkill] "actions" ":" [actionSkills] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing Nothing actionSkills soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        end ":" [endSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        end ":" [endSkill] counter ":" [counterSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        end ":" [endSkill] spawn ":" [spawnSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        end ":" [endSkill] death ":" [deathSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        end ":" [endSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        end ":" [endSkill] "action" ":" [actionSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        end ":" [endSkill] "actions" ":" [actionSkills] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing Nothing actionSkills soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
                        counter ":" [counterSkill] soul ":" [soulSkill] =
  MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing Nothing Nothing [] soulSkill

-}






























































{- I need to figure out what I want to do about this condition attached to skill factories... -}


{-Used should NOT be in SkillFactory.... It is always false ... -}







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





































































syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
death ":" [deathSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing (Just deathSkill) Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing Nothing Nothing [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
death ":" [deathSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing (Just deathSkill) Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing Nothing Nothing [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing Nothing Nothing [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
death ":" [deathSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing (Just deathSkill) Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing Nothing Nothing [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
death ":" [deathSkill] "auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing (Just deathSkill) (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
"auto" ":" [autoSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing Nothing (Just autoSkill) [] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
"auto" ":" [autoSkill] action ":" [actionSkill] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing Nothing (Just autoSkill) [actionSkill] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
"auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
"auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] end ":" [endSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) (Just endSkill) Nothing Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
start ":" [startSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) (Just startSkill) Nothing Nothing Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
end ":" [endSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing (Just endSkill) Nothing Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
counter ":" [counterSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing (Just counterSkill) Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
spawn ":" [spawnSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing (Just spawnSkill) Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
death ":" [deathSkill] "auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing (Just deathSkill) (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill

syntax [unit_name] "<-" [schools] lvl ":" [level] life ":" [hp] atk ":" [attack] def ":" [defense] spe ":" [speed] rng ":" [range] sp ":" [soulPoints]
"auto" ":" [autoSkill] action ":" [actionSkill1] action ":" [actionSkill2] action ":" [actionSkill3] action ":" [actionSkill4] soul ":" [soulSkill] =
MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) Nothing Nothing Nothing Nothing Nothing (Just autoSkill) [actionSkill1, actionSkill2, actionSkill3, actionSkill4] soulSkill












-}
