module Main.Spell_phase
import Base.Player
import Main.Game
import Base.Clientupdates
%access public export
%default total




stepSpellPhase : WhichPlayer -> List Nat -> Player -> Player -> (Game, List ClientUpdate)
stepSpellPhase initiative deathQueue player opponent = ?hole
