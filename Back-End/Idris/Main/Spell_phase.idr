module Main.Spell_phase
import Base.Preliminaries
import Base.Player
import Base.Skill_dsl_data
import Data.Fin
import Main.Game

import Base.Clientupdates
%access public export
%default total




{-getMessageSpellPhase : Player -> Player -> Nonautomatic -> Automatic -> ClientInstruction-}

-------------------------------------------------------------------------------

-- should this exist?
stepSpellPhase : WhichPlayer -> Nat -> List (Fin 25, WhichPlayer) -> Player -> Player -> (Game, List ClientUpdate, Maybe ClientInstruction)
stepSpellPhase initiative turnNumber deathQueue player opponent = ?hole
-------------------------------------------------------------------------------
{-
| SpellPhase = case transformSpellPhase actor (player_A game) (player_B game) (skillHead game) (skillQueue game) (deathQueue game) of
                     Right (errorMessage, playerId) => ?hole
                     Left ((playerA', playerB', skillHead', skillQueue', deathQueue'), updates) => ?hole
-}

-------------------------------------------------------------------------------
transformSpellPhase :
 (initiativePlayer : Player) ->
 (otherPlayer : Player) ->
 (skillHead : Nonautomatic) ->
 (skillQueue : List Automatic) ->
 (deathQueue : List (Fin 25)) ->
 Either
  (String, String) -- errorMessage, playerId
  (Player, Player, Nonautomatic, List Automatic, List Nat, List ClientUpdate)
-------------------------------------------------------------------------------

