module Main.Spell_phase
import Base.Player
import Main.Game
import Base.Clientupdates
%access public export
%default total




{-getMessageSpellPhase : Player -> Player -> Nonautomatic -> Automatic -> ClientInstruction-}

stepSpellPhase : WhichPlayer -> Nat -> List Nat -> Player -> Player -> (Game, List ClientUpdate, Maybe ClientInstruction)
stepSpellPhase initiative turnNumber deathQueue player opponent = ?hole
