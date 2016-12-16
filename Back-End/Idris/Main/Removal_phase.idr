module Main.Removal_phase
import Base.Player
import Main.Game
import Base.Clientupdates
%access public export
%default total




stepRemovalPhase : List Nat -> Player -> Player -> (Game, List ClientUpdate, Maybe ClientInstruction)
stepRemovalPhase deathQueue player opponent = ?hole
