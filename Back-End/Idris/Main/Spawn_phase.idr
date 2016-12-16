module Main.Spawn_phase
import Base.Player
import Main.Game
import Base.Clientupdates
%access public export
%default total




getMessageSpawnPhase : WhichPlayer -> Player -> Player -> ClientInstruction

stepSpawnPhase : WhichPlayer -> List Nat -> Player -> Player -> (Game, List ClientUpdate, Maybe ClientInstruction)
stepSpawnPhase initiative deathQueue player opponent = ?hole


