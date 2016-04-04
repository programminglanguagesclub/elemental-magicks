module Main

import Data.Vect
import Data.Fin
import Data.So
import preliminaries
import phase
import objects
import skill_dsl
import serverupdates
import clientupdates


record Game where
 constructor MkGame
 round : Bounded 0 1
 player_A_Initiative : Bool
 turnNumber : Nat
 skillHead : Maybe (Condition, SkillComponent, SkillComponent, SkillComponent)
 skillQueue : List SkillComponent
 player_A : Player 0 0
 player_B : Player 0 0
 phase : Phase
syntax "new" "game" [tokenA] [tokenB] = MkGame (0 ** Oh) True 0 (Vect.Nil,Vect.Nil,Vect.Nil) Nothing [] (new player tokenA) (new player tokenB) DrawPhase


Selection : {b : Nat} -> {h : Nat} -> {g : Nat} -> (game : Game) -> (Vect b BoardIndex, Vect h HandIndex, Vect g GraveyardIndex) -> (Game, List ClientUpdate)
Selection game (board, hand, graveyard) with (skillHead game)
 | Nothing = ?hole {-(game, [])-}
 | skill = ?hole

getSoulPoints : Player h g -> Nat
getSoulPoints player = ?hole

FooDrawCard : Player n m -> Card -> Player (S n) m
FooDrawCard player card = MkPlayer (board player) (reverse (card :: (reverse (hand player)))) (graveyard player) (spawn player) (soul player) (thoughts player) (knowledge player) (token player)

{-the following currently ignores client updates-}

executeSkillEffects : Game -> List SkillEffect -> Game
executeSkillEffects g a = ?hole

skillSelectionPossible : Game -> Condition -> Bool
skillSelectionPossible game condition = ?hole

stepGame : Game -> Game
stepGame g with (skillHead g, skillQueue g)
 | (Just (condition, ifSelects, SkillComponent_ (cannotSelectEffects, cannotSelectSkillHead) , next), skillQueue)
                                                                                     = if skillSelectionPossible g condition then g
                                                                                       else stepGame (executeSkillEffects (record {skillHead = cannotSelectSkillHead} g) cannotSelectEffects)
 | (Nothing, ((SkillComponent_ (skillEffects, next))::skillQueue))                   = stepGame (executeSkillEffects (record {skillHead = next} g) skillEffects)
 | (Nothing, []) with (round g, player_A_Initiative g, turnNumber g, player_A g, player_B g, phase g, getSoulPoints (player_A g), getSoulPoints (player_B g))
  | (round,player_A_Initiative,turnNumber,player_A,player_B,phase,Z,bsp)             = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,phase,asp,Z)             = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,phase,asp,bsp)           = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,DrawPhase,asp,bsp) with (spawn player_A, spawn player_B)
   | (Just cardA,Just cardB)                                                         = ?g {- record { phase = nextPhase DrawPhase } g -}
   | _                                                                               = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,SpawnPhase,asp,bsp)      = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,SpellPhase,asp,bsp)      = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,RemovalPhase,asp,bsp)    = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,StartPhase,asp,bsp)      = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,EngagementPhase,asp,bsp) = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,EndPhase,asp,bsp)        = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,RevivalPhase,asp,bsp)    = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,DeploymentPhase,asp,bsp) = ?g
  | _                                                                                = ?g

{-Need to cause units to leave the field if not revived in order of death, and then in order of position on the field. For this we need another data structure in game to represent the order of death-}

{-For now, completely ignore the possibility of the user using skills! :D -}

transformGame : Game -> ServerUpdate -> (Game, List ClientUpdate)
transformGame game serverupdate with (phase game,serverupdate)
 | (DrawPhase,DrawCard id)                = ?hole {-(game,[])-} {-Maybe-}
 | (DrawPhase,_)                          = ?hole {-(game,[])-} {-No-}
 | (SpawnPhase,SetCard schools cardIndex) = ?hole
 | (SpawnPhase,Skip schools)              = ?hole
 | (SpawnPhase, _)                        = ?hole
 | (SpellPhase,SkillSelection n)          = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (SpellPhase,_)                         = ?hole
 | (RemovalPhase,SkillSelection n)        = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (RemovalPhase,_)                       = ?hole
 | (StartPhase,SkillSelection n)          = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (StartPhase,_)                         = ?hole
 | (EngagementPhase, AttackRow n)         = ?hole
 | (EngagementPhase, Rest)                = ?hole
 | (EngagementPhase, DirectAttack)        = ?hole
 | (EngagementPhase, Move)                = ?hole
 | (EngagementPhase, SkillInitiation n)   = ?hole
 | (EngagementPhase, SkillSelection n)    = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (EngagementPhase,_)                    = ?hole
 | (EndPhase,SkillSelection n)            = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (EndPhase,_)                           = ?hole
 | (RevivalPhase,Revive b)                = ?hole
 | (RevivalPhase,_)                       = ?hole


while_loop : List Game -> ServerUpdate -> (List Game, List ClientUpdate)
while_loop [] _      = ?hole {-([],[])-}
while_loop (g::gs) _ = ?hole

main : IO ()
main = putStrLn "hello world"











