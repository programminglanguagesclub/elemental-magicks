module Main.Spell_phase
import Base.Preliminaries
import Base.Player
import Base.Skill_dsl_data
import Main.Game

import Base.Clientupdates
%access public export
%default total




{-getMessageSpellPhase : Player -> Player -> Nonautomatic -> Automatic -> ClientInstruction-}

-------------------------------------------------------------------------------

-- should this exist?
stepSpellPhase : WhichPlayer -> Nat -> List Nat -> Player -> Player -> (Game, List ClientUpdate, Maybe ClientInstruction)
stepSpellPhase initiative turnNumber deathQueue player opponent = ?hole
-------------------------------------------------------------------------------
{-
| SpellPhase = case transformSpellPhase actor (player_A game) (player_B game) (skillHead game) (skillQueue game) (deathQueue game) of
                     Right (errorMessage, playerId) => ?hole
                     Left ((playerA', playerB', skillHead', skillQueue', deathQueue'), updates) => ?hole
-}

-------------------------------------------------------------------------------
transformSpellPhase :
 WhichPlayer ->
 Player ->
 Player ->
 Nonautomatic ->
 List Automatic ->
 List Nat ->
 Either (String,String) (Player, Player, Nonautomatic, List Automatic, List Nat, List ClientUpdate)
-------------------------------------------------------------------------------

