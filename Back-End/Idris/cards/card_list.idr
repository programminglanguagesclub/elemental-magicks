module Card_list
import Data.Vect
import Data.So
import base.bounded
import base.bounded_then_integer
import base.integer_then_bounded
import base.hp
import base.preliminaries
import base.objects_basic
import base.skill_dsl_data
import base.skill_dsl_syntax
import base.phase
import base.clientupdates
import base.player
import base.card
import base.card_list_syntax





import water_8












%access public export
%default total




{-
{- reimaging this how I want it to be, assuming the syntax extensions work correctly
 I still have the superfluous specifications on whether the card can be dead.
 This will be fixed later.
-}

monsterList : List MonsterFactory
monsterList = [
  "Axeman" <- no_schools level : 3 life : 50 attack : 30 defense : 0 speed : 2 range : 1 sp : 2,
              soulSkill : "Brutal Strike" 2 thoughts select x in enemy board where not dead x then damage x 50
  "Goblin Berserker" <- .......
  
  
  ]




-}



mon : MonsterFactory
mon = "Axeman" <- NoSchools lvl : 3 life : 50 atk : 30 def : 0 spe : 2 rng : 1 sp : 2 soul : (done, 0, Vacuous)



ttt : AutomaticFactory
ttt = select x in enemy board where not dead x then { hp x := 0 } ;


testMonsterList : List MonsterFactory
testMonsterList = [
  "Axeman" <- no_schools lvl : 3 life : 50 atk : 30 def : 0 spe : 2 rng : 1 sp : 2 soul : (done, 0, Vacuous),
  "Rogue Assassin" <- no_schools lvl: 3 life: 30 atk: 30 def: 0 spe: 2 rng: 3 sp: 2
                      action : (select x in enemy board where not dead x then { hp x := 0 } ;, 0, Vacuous)
                      soul : (select x in enemy board where not dead x then { hp x := 0 } ;, 0, Vacuous),
  "Guardian Angel" <- no_schools lvl : 5 life : 50 atk : 30 def : 10 spe : 2 rng : 3 sp : 1 
                      spawn : (select x in friendly board where dead x then { revive x } ;,0, Vacuous)
                      soul : (done,0,Vacuous) {-(every x in friendly board where dead x do [revive x] next done, 0, Vacuous)-}{-,
   "Tank" <- no_schools lvl : 5 life : 45 atk : 40 def : 20 spe : 1 rng : 2 sp : 2
             soul : (every x in friendly board where not dead x do [permanenet defense x += 10] next done, 0, Vacuous),
   "Treant Watchman" <- earth lvl : 1 life : 40 atk : 10 def : 10 spe : 1 rng : 1 sp : 2,
                        counter : (done, 0, Vacuous) {- have to code this still -}
                        soul : (done, 0, Vacuous) {- have to code this still -}   -}
]

monsterList : List MonsterFactory
monsterList = [
   mon  {-,-} {-do 50 damage to x... eventually actually do this.-}
   
   
   {-"Goblin Berserker" <- no_schools lvl: 3 life: 40 atk: 30 def: 0 spe: 4 rng: 1 sp: 1-}
   
   {-,
   "Rogue Assassin" <- [action : 2 thoughts -> select x in enemy board where not dead x then hp x := 0 ;,
                        soulSkill : 2 thoughts -> select x in enemy board where not dead x then hp x := 0 ; ]
                       no_schools lvl: 3 life: 30 atk: 30 def: 0 spe: 2 rng: 3 sp: 2,
   "Guardian Angel" <- [spawnSkill : select x in friendly board where dead x then revive x ;,
                        soulSkill : all x in friendly board where dead x do [revive x] done]
                       no_schools lvl : 5 life : 50 atk : 30 def : 10 spe : 2 rng : 3 sp : 1,
   "Tank" <- [soulSkill : all x in friendly board where not dead x do [permanent defense x += 10] done]
              no_schools lvl : 5 life : 45 atk : 40 def : 20 spe : 1 rng : 2 sp : 2,
   "Treant Watchman" <- [counterSkill : done, soulSkill : done] {-NEED TO CODE THIS; make mass exhaustion target friendly field as well -}
                        earth lvl : 1 life : 40 atk : 10 def : 10 spe : 1 rng : 1 sp : 2,
   "Forest Druid" <- [action : 1 thoughts -> select x in friendly board where not dead x then [maxHp x := $ (SchoolR True 0) * 10, hp x := (maxHp x)] ;]
                     earth lvl : 2 life : 30 atk : 20 def : 0 spe : 1 rng : 2 sp : 2,
   "Forest Pixie" <- [action : 1 thoughts -> all x in friendly board where not dead x do [temporary attack x += $ (hp x)] done,
                      soulSkill : all x in friendly board where not dead x do [temporary attack x += $ (hp x)] done]
                     earth lvl : 2 life : 20 atk : 10 def : 0 spe : 5 rng : 3 sp : 2,
   "Fox Musician" <- [action : 0 thoughts -> all x in friendly board where not dead x do [permanent attack x += $ (Constant 5)] done]
                                                                           earth lvl : 2 life : 30 atk : 10 def : 0 spe : 2 rng : 3 sp : 2-}
]











{-
axeman : MonsterFactory
axeman = "Axeman" <-
         -}
         {-

monsterList : List MonsterFactory
monsterList = [
  "Axeman" <- [] no_schools lvl: 3 life: 50 atk: 30 def: 0 spe: 2 rng: 1 sp: 2,
  "Goblin Berserker" <- [] no_schools lvl: 3 life: 40 atk: 30 def: 0 spe: 4 rng: 1 sp: 1{-,
  "Rogue Assassin" <- [action : 2 thoughts -> select x in enemy board where not dead x then hp x := 0 ;,
                       soulSkill : 2 thoughts -> select x in enemy board where not dead x then hp x := 0 ; ]
                      no_schools lvl: 3 life: 30 atk: 30 def: 0 spe: 2 rng: 3 sp: 2,
  "Guardian Angel" <- [spawnSkill : select x in friendly board where dead x then revive x ;,
                       soulSkill : all x in friendly board where dead x do [revive x] done]
                      no_schools lvl : 5 life : 50 atk : 30 def : 10 spe : 2 rng : 3 sp : 1,
  "Tank" <- [soulSkill : all x in friendly board where not dead x do [permanent defense x += 10] done]
             no_schools lvl : 5 life : 45 atk : 40 def : 20 spe : 1 rng : 2 sp : 2,
  "Treant Watchman" <- [counterSkill : done, soulSkill : done] {-NEED TO CODE THIS; make mass exhaustion target friendly field as well -}
                       earth lvl : 1 life : 40 atk : 10 def : 10 spe : 1 rng : 1 sp : 2,
  "Forest Druid" <- [action : 1 thoughts -> select x in friendly board where not dead x then [maxHp x := $ (SchoolR True 0) * 10, hp x := (maxHp x)] ;]
                    earth lvl : 2 life : 30 atk : 20 def : 0 spe : 1 rng : 2 sp : 2,
  "Forest Pixie" <- [action : 1 thoughts -> all x in friendly board where not dead x do [temporary attack x += $ (hp x)] done,
                     soulSkill : all x in friendly board where not dead x do [temporary attack x += $ (hp x)] done]
                    earth lvl : 2 life : 20 atk : 10 def : 0 spe : 5 rng : 3 sp : 2,
  "Fox Musician" <- [action : 0 thoughts -> all x in friendly board where not dead x do [permanent attack x += $ (Constant 5)] done]
                                                                          earth lvl : 2 life : 30 atk : 10 def : 0 spe : 2 rng : 3 sp : 2-}
]


-}


{-I need to move more things to syntax extensions so that I can ditch some of these $s -}



{-


 "mutant pig" <- [] spirit_void life: 60 atk: 30 def: 0 spe: 2 rng: 1 lvl: 3 sp: 2,
  "greater succubus" <- [] spirit life: 60 atk: 0 def: 0 spe: 1 rng: 3 lvl: 3 sp: 2


-}
