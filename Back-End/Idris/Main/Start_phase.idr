module Main.Start_phase
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


transformStartPhase : WhichPlayer -> Player -> Player -> WhichPlayer -> Nonautomatic -> List Automatic -> List Nat ->
 Either (Player, Player, Nonautomatic, List Automatic, List Nat, List ClientUpdate) (String, String)
transformStartPhase = ?hole









