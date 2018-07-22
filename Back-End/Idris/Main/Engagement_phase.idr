module Main.Engagement_phase
import Data.Vect
import Data.Fin
import Base.Preliminaries
import Base.Objects_basic
import Base.Card
import Base.Player
import Main.Game
import Base.Clientupdates
import Base.Skill_dsl_data
%access public export
%default total



{-these can be refactored-}
_allUnitsDead : List (Maybe FieldedMonster) -> Bool
_allUnitsDead (Nothing::tl) = _allUnitsDead tl
_allUnitsDead ((Just m)::tl) with (aliveness (basic m))
 | Alive = False
 | DeadFresh = _allUnitsDead tl
 | DeadStale = _allUnitsDead tl
_allUnitsDead [] = True

allUnitsDead : Vect n (Maybe FieldedMonster) -> Bool
allUnitsDead board = _allUnitsDead (toList board)


-------------------------------------------------------------------------------
stepEngagementPhase :
 WhichPlayer ->
 List (Fin 25, WhichPlayer) ->
 Player ->
 Player ->
 (Game, List ClientUpdate, Maybe ClientInstruction)

stepEngagementPhase initiative deathQueue player opponent = ?hole

{-
 | EngagementPhase = case transformEngagementPhase actor (player_A game) (player_B game) (initiative game) (skillHead game) (skillQueue game) (deathQueue game) of
                     Right (errorMessage, playerId) => ?hole
                     Left (playerA', playerB', skillHead', skillQueue', deathQueue',updates) => ?hole

-}

-------------------------------------------------------------------------------


transformEngagementPhase : -- NEED to also pass in the server update to this function, right????
 WhichPlayer ->
 Player ->
 Player ->
 WhichPlayer ->
 Nonautomatic ->
 List Automatic ->
 List (Fin 25, WhichPlayer) ->
 Either (String, String) (Player, Player, Nonautomatic, List Automatic, List (Fin 25, WhichPlayer), List ClientUpdate)
