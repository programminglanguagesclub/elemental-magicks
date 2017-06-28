module Main.Transform_game

{-import Data.Vect
import Data.So
import Base.Preliminaries
-}
import Data.Fin
import Base.Phase
{-import Base.Objects_basic-}
import Base.Preliminaries
import Base.Skill_dsl_data
import Base.Player
import Main.Game
import Main.Serverupdates
import Base.Clientupdates
import Main.Draw_phase
import Main.Spawn_phase
import Main.Spell_phase
import Main.Removal_phase
import Main.Start_phase
import Main.Engagement_phase
import Main.End_phase
import Main.Revival_phase
import Main.Deployment_phase
import Main.Transform_game_helpers
import Main.Step_game


%access public export
%default total


{- if two units die the same time on the same number square, have the unit of the player with the initiative die first -}

{-returned values need to allow for transitions between phases causing further changes to the game outside of fields set in the current phase...-}
-------------------------------------------------------------------------------
partial
transformGame' :
 Game ->
 WhichPlayer ->
 ServerUpdate ->
 (Either WhichPlayer Game, List ClientUpdate)

transformGame' game actor serverUpdate with (phase game) {-I'm going to pass in phase even though it's bad form, just so I don't have to reconstruct game for now-}
 | _ = ?hole
{- | DrawPhase playerA playerB cardsDrawn =
   case transformDrawPhase actor playerA playerB serverUpdate of
    Nothing => (Right game,[GameLogicError])
    Just $ Left (errorMessage, playerId) => (Right game,[InvalidMove errorMessage playerId])
    Just $ Right (player', updates) =>
     case actor of
      PlayerA => (Right $ record {phase = DrawPhase player' playerB ?hole} game, updates)
      PlayerB => (Right $ record {phase = DrawPhase playerA player' ?hole} game, updates)
 -}
{-
 | SpawnPhase =
   case transformSpawnPhase actor playerA playerB (initiative game) serverUpdate of
    Left (errorMessage, playerId) => (Right game,[InvalidMove errorMessage playerId])
    Right ((playerA', playerB'), updates) => ?hole
 
 | MkPhaseCycle SpellPhase playerA playerB =
   case transformSpellPhase actor playerA playerB (skillHead game) (skillQueue game) (deathQueue game) of
    Left (errorMessage, playerId) => (Right game,[InvalidMove errorMessage playerId])
    Right (playerA', playerB', skillHead', skillQueue', deathQueue', updates) => ?hole
 
 | MkPhaseCycle RemovalPhase playerA playerB =
   case transformRemovalPhase actor playerA playerB (skillHead game) (skillQueue game) (deathQueue game) of
    Left (errorMessage, playerId) => (Right game,[InvalidMove errorMessage playerId])
    Right (playerA', playerB', skillHead', skillQueue', deathQueue', updates) => ?hole
 
 | MkPhaseCycle StartPhase playerA playerB =
   case transformStartPhase actor playerA playerB (initiative game) (skillHead game) (skillQueue game) (deathQueue game) of
    Left (errorMessage, playerId) => (Right game,[InvalidMove errorMessage playerId])
    Right (playerA', playerB', skillHead', skillQueue', deathQueue',updates) => ?hole
 
 | MkPhaseCycle EngagementPhase playerA playerB =
   case transformEngagementPhase actor playerA playerB (initiative game) (skillHead game) (skillQueue game) (deathQueue game) of
    Left (errorMessage, playerId) => (Right game,[InvalidMove errorMessage playerId])
    Right (playerA', playerB', skillHead', skillQueue', deathQueue',updates) => ?hole
 
 | MkPhaseCycle EndPhase playerA playerB =
   case transformEndPhase actor playerA playerB (initiative game) (skillHead game) (skillQueue game) (deathQueue game) of
    Left (errorMessage, playerId) => (Right game,[InvalidMove errorMessage playerId])
    Right (playerA', playerB', skillHead', skillQueue', deathQueue',updates) => ?hole
 
 | MkPhaseCycle RevivalPhase playerA playerB =
   case transformRevivalPhase actor playerA playerB (initiative game) (deathQueue game) of
    Left (errorMessage, playerId) => (Right game,[InvalidMove errorMessage playerId])
    Right (playerA', playerB', deathQueue', updates) => ?hole

-}
{- for now, because I have holes everywhere, just assert this is total so we can get the draw phase tested -}


transformGame : Game -> WhichPlayer -> ServerUpdate -> (Either WhichPlayer Game, List ClientUpdate)
transformGame = assert_total transformGame'










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

-------------------------------------------------------------------------------
transformDrawPhase :
 DrawPhase ->
 WhichPlayer ->
 ServerUpdate ->
 (FullGame, List ClientUpdate)

-------------------------------------------------------------------------------
transformFullGame :
 FullGame ->
 WhichPlayer ->
 ServerUpdate ->
 (Either WhichPlayer FullGame, List ClientUpdate)

-------------------------------------------------------------------------------


