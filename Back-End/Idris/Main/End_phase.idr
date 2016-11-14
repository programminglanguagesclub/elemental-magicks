module Main.End_phase
import Base.Player
import Main.Game
import Base.Clientupdates
%access public export
%default total


stepEndPhase : WhichPlayer -> List Nat -> Player -> Player -> (Game, List ClientUpdate)
stepEndPhase initiative deathQueue player opponent = ?hole
