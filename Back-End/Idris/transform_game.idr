module Transform_game

import Data.Vect
import Data.So
import preliminaries
import phase
import objects_basic
import skill_dsl
import skill_dsl_data
import player
import game
import serverupdates
import clientupdates
import draw_phase
import spawn_phase
import spell_phase
import removal_phase
import start_phase
import engagement_phase
import end_phase
import revival_phase
import deployment_phase
import transform_game_helpers
import step_game
import step_game_helpers
%access public export
%default total









{-









{-Somewhere I also have to return whose turn it is.-}

{-might want to refactor this type into a binary datatype and a server update so that I don't have a fail case that I already ruled out... (no player with that temporaryId)-}
{-transformGame : Game -> ServerUpdateWrapper -> (Game, List ClientUpdate)-}
public export
transformGame : Game -> (player : Player) -> (opponent : Player) -> WhichPlayer -> ServerUpdate -> (Game, WhichPlayer, List ClientUpdate)
transformGame game player opponent whichPlayer serverUpdate = ?hole


{-with (phase game,serverUpdate)
 | (DrawPhase,DrawCard id)                = ?hole {-(game,[])-} {-Maybe-}
 | (DrawPhase,_)                          = (game, whichPlayer, [(InvalidMove (temporaryId player))])
 | (SpawnPhase,SetCard schools cardIndex) with (index' cardIndex (hand player))        {-Also have to make sure it's the player's turn!!-}
  | Nothing = (game, whichPlayer, [GameLogicError])                {-well, right now the user can input too large a number, but this will be a logic error once that is fixed-}
  | Just (MonsterCard card) = if schoolsHighEnoughToPlayCard player (MonsterCard card) {-THIS DOES NOT ACTUALLY CHECK IF THE SCHOOLS ARE HIGH ENOUGH AFTER REGISTERING THE RAISE SCHOOLS CHANGE-}
                               then
                                let spawn' = index' cardIndex (hand player) in
                                let hand' = removeAt (hand player) cardIndex in
                                let thoughts' = (extractBounded (thoughts player)) - (extractBounded (getBaseLevel (level (basic card)))) in
                                {-let schools' = ?g in-}
                                ?g
                               else
                               (game, [InvalidMove (temporaryId player)])
  | Just (SpellCard card) = if schoolsHighEnoughToPlayCard player (SpellCard card)
                             then
                              let hand' = removeAt (hand player) cardIndex in
                              let thoughts' = (extractBounded (thoughts player)) - (extractBounded (level (basic card))) in
                              ?g
                             else
                              (game, [InvalidMove (temporaryId player)])



{-
                let pu = \p => p in
                                let game' = updatePlayer game player in

-}

{-Also have to make sure it's the player's turn!!-}
 | (SpawnPhase,Skip schools)              = if (dominatesVect maxSchools schools) && (dominatesVect schools (knowledge player)) && (totalDifferenceVect schools (knowledge player) <= extractBounded (thoughts player))
                                             then (updateGame game player, [UpdateSchools schools (temporaryId player) (temporaryId opponent)])
                                             else (game, [InvalidMove (temporaryId player)])
 | (SpawnPhase,_)                         = (game, [InvalidMove (temporaryId player)])
 | (SpellPhase,SkillSelection s)          = handleSkillSelection game s
 | (SpellPhase,_)                         = (game, [InvalidMove (temporaryId player)])
 | (RemovalPhase,SkillSelection s)        = handleSkillSelection game s
 | (RemovalPhase,_)                       = (game, [InvalidMove (temporaryId player)])
 | (StartPhase,SkillSelection s)          = handleSkillSelection game s
 | (StartPhase,_)                         = (game, [InvalidMove (temporaryId player)])
 | (EngagementPhase,AttackRow n) with (engagementOnMove game player opponent)
  | (False,_)                             = (game, [GameLogicError]) {-assume we already filter for whose turn it is in Ur/Web-}
  | (True,i) with (inRangeRow (board player) (board opponent) i n)
   | Nothing                              = (game, [GameLogicError])
   | Just False                           = (game, [InvalidMove (temporaryId player)])
   | Just True                            = ?g {-NEED TO ALSO MAKE SURE THAT THERE IS A VALID TARGET IN THE ROW (NOT JUST THAT IT IS IN RANGE)-}
 | (EngagementPhase,Rest) with (skillHead game, skillQueue game)
  | (Nothing, []) with (engagementOnMove game player opponent)
   | (False,_)                            = (game, [GameLogicError])
   | (True,location)                      = restUnit location game player
  | (_,_)                                 = (game, [InvalidMove(temporaryId player)])
 | (EngagementPhase,DirectAttack) with (skillHead game, skillQueue game)
  | (Nothing, []) with (allUnitsDead (board opponent))
   | False                                = (game, [InvalidMove (temporaryId player)])
   | True                                 = ?g {-with (engagementOnMove (board))      ... Need to make sure that the user can move with the card that is attacking, and that they have at least 1 thought...-}
  | (_,_)                                 = (game, [InvalidMove (temporaryId player)])                 {-   ?hole {-how do I get the player. Should that be passed instead of just the temporaryId? Also call all units dead.-}-}
 | (EngagementPhase,Move moveTo) with (engagementOnMove game player opponent)
  | (False,_)                             = (game, [GameLogicError]) {-assume we already filter for whose turn it is in Ur/Web-}
  | (True,moveFrom) with (index moveTo(board (player)))
   | Nothing with (whichPlayer)
    | PlayerA = ((record {player_A->board = moveUnit moveFrom moveTo (board player)} game), [MoveUnit moveFrom moveTo (temporaryId player) (temporaryId opponent)])
    | PlayerB = ((record {player_B->board = moveUnit moveFrom moveTo (board player)} game), [MoveUnit moveFrom moveTo (temporaryId player) (temporaryId opponent)])
   | Just monster                         = (game, [InvalidMove (temporaryId player)]) {-Can't move to a location with something-}
 | (EngagementPhase,SkillInitiation n)    = handleSkillInitiation game n
 | (EngagementPhase,SkillSelection s)     = handleSkillSelection game s
 | (EngagementPhase,_)                    = (game, [InvalidMove (temporaryId player)])
 | (EndPhase,SkillSelection s)            = handleSkillSelection game s {-In all of these handleSkillSelection and handleSkillInitiation have to make sure that the right player is moving, that there is/isn't a pending skill, etc.-}
 | (EndPhase,_)                           = (game, [InvalidMove (temporaryId player)])
 | (RevivalPhase,Revive b)                = ?g {-if canRevive player b
                                             then ?g
                                             else (game, [InvalidMove (temporaryId player)])-}
 | (RevivalPhase,_)                       = (game, [InvalidMove (temporaryId player)])
-}














-}
