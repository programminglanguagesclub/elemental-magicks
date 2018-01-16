module Cards.Card_list
import Data.Vect
import Data.So
import Base.Bounded
import Base.Preliminaries
import Base.Skill_dsl_data
import Base.Objects_basic
import Base.Skill_dsl_syntax
import Base.Card
import Base.Card_list_syntax

%access public export
%default total

-------------------------------------------------------------------------------
axeman : MonsterFactory
axeman = ?hole
-------------------------------------------------------------------------------


cardList : List CardFactory
cardList = [MonsterCardFactory axeman]

testMonsterList : List MonsterFactory
testMonsterList = ?hole


{-

monsterList : List MonsterFactory
monsterList = [
  "Axeman" <- no_schools level : 3 life : 50 attack : 30 defense : 0 speed : 2 range : 1 sp : 2,
              soulSkill : "Brutal Strike" 2 thoughts select x in enemy board where not dead x then damage x 50
  "Goblin Berserker" <- .......
  
  
  ]




   

   "Rogue Assassin" <- [action : 2 thoughts -> select x in enemy board where not dead x then hp x := 0 ;,
                        soulSkill : 2 thoughts -> select x in enemy board where not dead x then hp x := 0 ; ]
                       no_schools lvl: 3 life: 30 atk: 30 def: 0 spe: 2 rng: 3 sp: 2,
   "Guardian Angel" <- [spawnSkill : select x in friendly board where dead x then revive x ;,
                        soulSkill : all x in friendly board where dead x do [revive x] done]
                       no_schools lvl : 5 life : 50 atk : 30 def : 10 spe : 2 rng : 3 sp : 1,
   "Tank" <- [soulSkill : all x in friendly board where not dead x do [permanent defense x += 10] done]
              no_schools lvl : 5 life : 45 atk : 40 def : 20 spe : 1 rng : 2 sp : 2,
   "Treant Watchman" <- [counterSkill : done, soulSkill : done] --NEED TO CODE THIS; make mass exhaustion target friendly field as well
                        earth lvl : 1 life : 40 atk : 10 def : 10 spe : 1 rng : 1 sp : 2,
   "Forest Druid" <- [action : 1 thoughts -> select x in friendly board where not dead x then [maxHp x := $ (SchoolR True 0) * 10, hp x := (maxHp x)] ;]
                     earth lvl : 2 life : 30 atk : 20 def : 0 spe : 1 rng : 2 sp : 2,
   "Forest Pixie" <- [action : 1 thoughts -> all x in friendly board where not dead x do [temporary attack x += $ (hp x)] done,
                      soulSkill : all x in friendly board where not dead x do [temporary attack x += $ (hp x)] done]
                     earth lvl : 2 life : 20 atk : 10 def : 0 spe : 5 rng : 3 sp : 2,
   "Fox Musician" <- [action : 0 thoughts -> all x in friendly board where not dead x do [permanent attack x += $ (Constant 5)] done]
                                                                           earth lvl : 2 life : 30 atk : 10 def : 0 spe : 2 rng : 3 sp : 2

 "mutant pig" <- [] spirit_void life: 60 atk: 30 def: 0 spe: 2 rng: 1 lvl: 3 sp: 2,
  "greater succubus" <- [] spirit life: 60 atk: 0 def: 0 spe: 1 rng: 3 lvl: 3 sp: 2


-}

