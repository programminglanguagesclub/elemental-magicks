module Transform_game

import Data.Vect
import preliminaries
import phase
import objects_basic
import skill_dsl
import objects_advanced
import serverupdates
import clientupdates
import transform_game_helpers
import step_game
import step_game_helpers


{-Somewhere I also have to return whose turn it is.-}

{-might want to refactor this type into a binary datatype and a server update so that I don't have a fail case that I already ruled out... (no player with that token)-}
{-transformGame : Game -> ServerUpdateWrapper -> (Game, List ClientUpdate)-}
public export
transformGame : Game -> (player : Player) -> (opponent : Player) -> WhichPlayer -> ServerUpdate -> (Game, List ClientUpdate)
transformGame game player opponent whichPlayer serverUpdate with (phase game,serverUpdate)
 | (DrawPhase,DrawCard id)                = ?hole {-(game,[])-} {-Maybe-}
 | (DrawPhase,_)                          = (game, [(InvalidMove (token player))])
 | (SpawnPhase,SetCard schools cardIndex) = ?hole
 | (SpawnPhase,Skip schools)              = if (dominatesVect maxSchools schools) && (dominatesVect schools (knowledge player)) && (totalDifferenceVect schools (knowledge player) <= extractBounded (thoughts player))
                                             then (updateGame game player, [UpdateSchools schools (token player) (token opponent)])
                                             else (game, [InvalidMove (token player)])
 | (SpawnPhase,_)                         = (game, [InvalidMove (token player)])
 | (SpellPhase,SkillSelection s)          = handleSkillSelection game s
 | (SpellPhase,_)                         = (game, [InvalidMove (token player)])
 | (RemovalPhase,SkillSelection s)        = handleSkillSelection game s
 | (RemovalPhase,_)                       = (game, [InvalidMove (token player)])
 | (StartPhase,SkillSelection s)          = handleSkillSelection game s
 | (StartPhase,_)                         = (game, [InvalidMove (token player)])
 | (EngagementPhase,AttackRow n) with (engagementOnMove game player opponent)
  | (False,_)                             = (game, [GameLogicError]) {-assume we already filter for whose turn it is in Ur/Web-}
  | (True,i) with (inRangeRow (board player) (board opponent) i n)
   | Nothing                              = (game, [GameLogicError])
   | Just False                           = (game, [InvalidMove (token player)])
   | Just True                            = ?g {-NEED TO ALSO MAKE SURE THAT THERE IS A VALID TARGET IN THE ROW (NOT JUST THAT IT IS IN RANGE)-}
 | (EngagementPhase,Rest) with (skillHead game, skillQueue game)
  | (Nothing, []) with (engagementOnMove game player opponent)
   | (False,_)                            = (game, [GameLogicError])
   | (True,location)                      = restUnit location game player
  | (_,_)                                 = (game, [InvalidMove(token player)])
 | (EngagementPhase,DirectAttack) with (skillHead game, skillQueue game)
  | (Nothing, []) with (allUnitsDead (board opponent))
   | False                                = (game, [InvalidMove (token player)])
   | True                                 = ?g {-with (engagementOnMove (board))      ... Need to make sure that the user can move with the card that is attacking, and that they have at least 1 thought...-}
  | (_,_)                                 = (game, [InvalidMove (token player)])                 {-   ?hole {-how do I get the player. Should that be passed instead of just the token? Also call all units dead.-}-}
 | (EngagementPhase,Move moveTo) with (engagementOnMove game player opponent)
  | (False,_)                             = (game, [GameLogicError]) {-assume we already filter for whose turn it is in Ur/Web-}
  | (True,moveFrom) with (index moveTo(board (player)))
   | Nothing with (whichPlayer)
    | PlayerA = ((record {player_A->board = moveUnit moveFrom moveTo (board player)} game), [MoveUnit moveFrom moveTo (token player) (token opponent)])
    | PlayerB = ((record {player_B->board = moveUnit moveFrom moveTo (board player)} game), [MoveUnit moveFrom moveTo (token player) (token opponent)])
   | Just monster                         = (game, [InvalidMove (token player)]) {-Can't move to a location with something-}
 | (EngagementPhase,SkillInitiation n)    = handleSkillInitiation game n
 | (EngagementPhase,SkillSelection s)     = handleSkillSelection game s
 | (EngagementPhase,_)                    = (game, [InvalidMove (token player)])
 | (EndPhase,SkillSelection s)            = handleSkillSelection game s {-In all of these handleSkillSelection and handleSkillInitiation have to make sure that the right player is moving, that there is/isn't a pending skill, etc.-}
 | (EndPhase,_)                           = (game, [InvalidMove (token player)])
 | (RevivalPhase,Revive b)                = if canRevive player b
                                             then ?g
                                             else (game, [InvalidMove (token player)])
 | (RevivalPhase,_)                       = (game, [InvalidMove (token player)])
