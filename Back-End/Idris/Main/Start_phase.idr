module Main.Start_phase
import Data.Fin
import Base.Preliminaries
import Base.Player
import Main.Game
import Base.Clientupdates
import Base.Skill_dsl_data
%access public export
%default total



-------------------------------------------------------------------------------
stepStartPhase :
 WhichPlayer ->
 List (Fin 25, WhichPlayer) ->
 Player ->
 Player ->
 (Game, List ClientUpdate, Maybe ClientInstruction)

stepStartPhase initiative deathQueue player opponent = ?hole


{-
| StartPhase = case transformStartPhase actor (player_A game) (player_B game) (initiative game) (skillHead game) (skillQueue game) (deathQueue game) of
                     Right (errorMessage, playerId) => ?hole
                     Left (playerA', playerB', skillHead', skillQueue', deathQueue',updates) => ?hole
-}
-------------------------------------------------------------------------------
transformStartPhase : -- ALSO NEED server update to be passed in right!!?!??!?!?
 WhichPlayer ->
 Player ->
 Player ->
 WhichPlayer ->
 Nonautomatic ->
 List Automatic ->
 List (Fin 25, WhichPlayer) ->
 Either (String,String) (Player, Player, Nonautomatic, List Automatic, List (Fin 25, WhichPlayer), List ClientUpdate)

transformStartPhase = ?hole
-------------------------------------------------------------------------------








