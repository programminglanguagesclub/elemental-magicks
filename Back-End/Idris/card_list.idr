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
import card
import card_list_syntax
%access public export
%default total

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

{-I need to move more things to syntax extensions so that I can ditch some of these $s -}



{-


 "mutant pig" <- [] spirit_void life: 60 atk: 30 def: 0 spe: 2 rng: 1 lvl: 3 sp: 2,
  "greater succubus" <- [] spirit life: 60 atk: 0 def: 0 spe: 1 rng: 3 lvl: 3 sp: 2


-}
