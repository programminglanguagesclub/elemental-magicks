module Water_8
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


{-

unit_list_water_8 : MonsterFactory
unit_list_water_8 = [
 "Water Titan" <- water lvl : 8 life : 180 atk : 65 def : 0 spe : 1 rng : 5 sp : 2
  soul : (done,0,Vacuous)                    

                     {- 
                      spawn : (select x in friendly board where dead x then { revive x } ;,0, Vacuous)
                      soul : (done,0,Vacuous) (every x in friendly board where dead x do [revive x] next done, 0, Vacuous)-}
]



-}
