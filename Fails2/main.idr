module Main

import Data.Vect
import Data.Fin
import Data.So
import preliminaries
import objects
import skill_dsl


record Game where
 constructor MkGame
 round : Bounded 0 1
 m : Nat
 n : Nat
 p : Nat

 env : Env m n p
 empty_env : Env 0 0 0
 skillHead : Maybe (Skill m n p env)
 skillQueue : List (NonautomaticSkillComponent 0 0 0 empty_env)
 player_A : Player 0 0
 player_B : Player 0 0


main : IO ()
main = putStrLn "Wah!"








