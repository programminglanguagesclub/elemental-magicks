module Main.Spell_phase
import Base.Player
import Main.Game
import Base.Clientupdates
%access public export
%default total




stepSpellPhase : WhichPlayer -> Nat -> List Nat -> Player -> Player -> (Game, List ClientUpdate)
stepSpellPhase initiative turnNumber deathQueue player opponent = ?hole
