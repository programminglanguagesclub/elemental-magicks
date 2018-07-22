module Main.Removal_phase
import Data.Fin
import Base.Preliminaries
import Base.Player
import Main.Game
import Base.Clientupdates
import Base.Skill_dsl_data

%access public export
%default total



-------------------------------------------------------------------------------
stepRemovalPhase :
 List (Fin 25, WhichPlayer) ->
 Player ->
 Player ->
 (Game, List ClientUpdate, Maybe ClientInstruction)

stepRemovalPhase deathQueue player opponent = ?hole


{-

| RemovalPhase = case transformRemovalPhase actor (player_A game) (player_B game) (skillHead game) (skillQueue game) (deathQueue game) of
                       Right (errorMessage, playerId) => ?hole
                       Left (playerA', playerB', skillHead', skillQueue', deathQueue', updates) => ?hole


-}

-------------------------------------------------------------------------------


transformRemovalPhase : -- Again, need update argument....!?!?
 WhichPlayer ->
 Player ->
 Player ->
 Nonautomatic ->
 List Automatic ->
 List (Fin 25, WhichPlayer) ->
 Either (String, String) (Player, Player, Nonautomatic, List Automatic, List (Fin 25, WhichPlayer), List ClientUpdate)


