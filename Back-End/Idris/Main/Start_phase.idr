module Main.Start_phase
import Base.Preliminaries
import Base.Player
import Main.Game
import Base.Clientupdates
import Base.Skill_dsl_data
%access public export
%default total




stepStartPhase : WhichPlayer -> List Nat -> Player -> Player -> (Game, List ClientUpdate, Maybe ClientInstruction)
stepStartPhase initiative deathQueue player opponent = ?hole


{-
| StartPhase = case transformStartPhase actor (player_A game) (player_B game) (initiative game) (skillHead game) (skillQueue game) (deathQueue game) of
                     Right (errorMessage, playerId) => ?hole
                     Left (playerA', playerB', skillHead', skillQueue', deathQueue',updates) => ?hole
-}


transformStartPhase :
 WhichPlayer ->
 Player ->
 Player ->
 WhichPlayer ->
 Nonautomatic ->
 List Automatic ->
 List Nat ->
 Either (String,String) (Player, Player, Nonautomatic, List Automatic, List Nat, List ClientUpdate)

transformStartPhase = ?hole









