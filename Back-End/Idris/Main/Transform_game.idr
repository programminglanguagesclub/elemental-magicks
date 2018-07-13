module Main.Transform_game
import Data.Vect
import Data.Fin
import Base.BoundedList
import Base.Bounded
import Base.Phase
import Base.Preliminaries
import Base.Skill_dsl_data
import Base.Player
import Base.Card
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

myNot : Bool -> Bool
myNot True = False
myNot False = True

partial
transformGame'' :
 Player ->
 Phase ->
 ServerUpdate ->
 Either
  String
  (Player,List ClientUpdate)

transformGame'' player phase update =
 case phase of
  SpawnPhase => ?hole
  SpellPhase => ?hole
  RemovalPhase => ?hole
  StartPhase => ?hole
  EngagementPhase => ?hole
  EndPhase => ?hole
  RevivalPhase => ?hole
  DeploymentPhase => ?hole




-- hack for now
getMonsterOnMove : FieldedMonster
getSquareOnMove : Fin 9
damageSoul : BoundedList 5 SoulCard -> (BoundedList 5 SoulCard, Maybe (Maybe Skill))



partial
transformGame' :
 Game ->
 WhichPlayer ->
 ServerUpdate ->
 (Either WhichPlayer Game, List ClientUpdate) -- either winning player or the game


-- I can make a nice monad for this...

-- NEED TO STEP GAME TOO?


transformGame' game actor serverUpdate =
 case (playerOnMove game == actor) of
  False => (Right game, [InvalidMove Clientupdates.notYourTurn actor])
  True =>
   let (player, mutator) = getStatefulPlayer actor game in
   case (phase game) of
    SpawnPhase =>
     case transformSpawnPhase player serverUpdate of
      Left errorMessage => (Right game, [InvalidMove errorMessage actor])
      Right (player', updates) =>
       case (getInitiative game == playerOnMove game) of
        True =>
         let (game', updates', instruction) = stepGame (mutator player' game, updates) in
         ?hole -- need to add instruction to updates for both players
        False =>
         let phase' = nextPhase (phase game) in
         let (game', updates', instruction) = stepGame (mutator player' (record {phase = phase'} game), updates) in
         ?hole -- need to add instruction to updates for both players
    EngagementPhase =>
     case (skillHead game) of



           -- THIS IS ONE BIG THING

      TerminatedSkill =>
       case serverUpdate of
        DirectAttack =>
         let enemyFieldNoneAlive = ?hole in
         case enemyFieldNoneAlive of
          True =>
           case (thoughtsResource player > 0) of
            False => (Right game, [InvalidMove "Invalid move. Direct attacks consume 1 thought. You have 0 thoughts, and cannot afford this cost." actor])
            True =>
             let player' = record {thoughtsResource $= \x => x - 1} player in
             let player'' = record {board $= ?hole {-engage unit that is direct attacking!!-}} player' in
             let (opponent, opponentMutator) = getStatefulPlayer (getOpponent actor) game in
             let x = 56 in
             let opponent' = record {soulCards = soulCards opponent} opponent {-decrease soul points-} in ?hole {-shoot. I also have to get whether or not to put a skill on the queue...-}

                   -- ?hole -- actually damage enemy soul by 1 for a cost of 1 thought(and consume card turn)
          False =>  (Right game, [InvalidMove "Invalid move. Direct attacks are only possible if there are no living units in the enemy field." actor])
        AttackRow row => ?hole -- don't allow any attack actions if nothing is in range (cannot even waste turn attacking), so just check to see if row is in range
        Rest => ?hole -- this is always valid
        Move fieldIndex => ?hole -- this is valid if the fieldIndex given is empty
        SkillInitiation skillIndex =>
         case (muted getMonsterOnMove) of
          True => (Right game, [InvalidMove "Invalid move. Your card cannot initiate action skills its first turn fielded" actor])
          False => ?hole -- 
        _ => (Right game, [InvalidMove "Invalid move. It is currently the action phase of your card. Please select a valid action for it." actor])
                       -- ?hole -- process engagementPhase specific
      Existential selection condition ifSelected ifUnable cost s => -- what is the last argument??? I have no idea anymore
       case serverUpdate of

            -- THIS IS ONE BIG THING

        SkillSelection friendlyField enemyField friendlyHand enemyHand friendlyGraveyard enemyGraveyard => ?hole
        _ => (Right game, [InvalidMove "Invalid move. Select targets for your current skill." actor])
    RevivalPhase => -- I need to make sure when I enter the revive phase I skip over if nobody can revive.
     case transformRevivalPhase player (deathQueue game) serverUpdate of
      Left errorMessage => (Right game, [InvalidMove errorMessage actor])
      Right (player', deathQueue', updates) =>                  -- NEED HELPER HERE FOR CAN REVIVE ANYTHING..
       case myNot $ getInitiative game == playerOnMove game || ?hole {-first player cannot revive anything-} of
        True =>
         let phase' = nextPhase (phase game) in
         let (game', updates', instruction) = stepGame (mutator player' (record {phase = phase', deathQueue = deathQueue'} game), updates) in
         ?hole
        False =>
         let (game', updates', instruction) = stepGame (mutator player' (record {deathQueue = deathQueue'} game), updates) in
         ?hole
    DeploymentPhase =>
     case transformDeploymentPhase player serverUpdate of
      Left errorMessage => (Right game, [InvalidMove errorMessage actor])
      Right (player', updates) =>
       case (getInitiative game == playerOnMove game) of
        True =>
         let (game', updates', instruction) = stepGame (mutator player' game, updates) in
         ?hole
        False =>
         let phase' = nextPhase (phase game) in
         let (game', updates', instruction) = stepGame (mutator player' (record {phase = phase'} game), updates) in
         ?hole
    _ =>
     case serverUpdate of
      SkillSelection friendlyField enemyField friendlyHand enemyHand friendlyGraveyard enemyGraveyard => ?hole
      _ => (Right game, [InvalidMove "Invalid move. Select targets for your current skill." actor])
   
   {-
       case transformGame'' player (phase game) serverUpdate {- (myNot (getInitiative game == playerOnMove game)) -} of
    Left errorMessage => ?hole
    Right (player', updates) =>
    let game' = (case phase game of _ => 1) in
    let (game'', (updates', clientInstruction)) = stepGame (mutator player' game, updates) in
    (Right game'', updates ++ updates' ++ ?hole) -- append the instruction..
    -}
     ---- if I assume I am always in the same phase after the transform, how do I tell
             -- the difference between the spawn phase where nobody has moved and the spawn phase after both skipped,
                                                             -- assuming I am only keeping track of the move by a single boolean flag
                                                              -- This is only an issue for spawn and revival phases
                                                              -- of these, spawn always requires a valid command from both players
                                                              -- and revival requires a valid command from each player who can revive...

{-

transformSpawnPhase : -- assumes the player is on move.
                       (playerToUpdate : Player) ->
                        (serverUpdate : ServerUpdate) ->
                         Either
                           String
                             (Player,List ClientUpdate)

                             -}


{-| _ = ?hole-}

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
-------------------------------------------------------------------------------
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
