module Main.Transform_game
--import Control.ST
import Data.So
import Data.Vect
import Data.Fin
import Base.Hp
import Base.BoundedList
import Base.Bounded
import Base.Phase
import Base.Preliminaries
import Base.Skill_dsl_data
import Base.Skill_dsl_logic
import Base.Player
import Base.Objects_basic
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



-- hack for now
getMonsterOnMove : FieldedMonster
getSquareOnMove : Fin 9
damageSoul : BoundedList 5 SoulCard -> (BoundedList 5 SoulCard, Maybe (Maybe Skill))


data Three error terminal continue
 = Left error
 | Center terminal
 | Right continue

partial
transformGame' :
 Game ->
 WhichPlayer ->
 ServerUpdate ->
 Three String (WhichPlayer, List ClientUpdate) (Game, List ClientUpdate, ClientInstruction) -- user error, winning player or the game
 --ignoring game logic error for the moment...

-- NEED TO STEP GAME TOO?

transformGame' game actor serverUpdate =
 case (playerOnMove game == actor) of
  False => Left Clientupdates.notYourTurn
  True =>
   let (player, mutator) = getStatefulPlayer actor game in
   let (opponent, opponentMutator) = getStatefulPlayer (getOpponent actor) game in
   case (phase game) of
    SpawnPhase =>
     case transformSpawnPhase player serverUpdate of
      Left errorMessage => Left errorMessage
      Right (player', updates) =>
       case (getInitiative game == playerOnMove game) of
        True =>
         Right $ stepGame (mutator player' game, updates)
        False =>
         let phase' = nextPhase (phase game) in
         Right $ stepGame (mutator player' (record {phase = phase'} game), updates)
    EngagementPhase (fieldedMonster, fieldedMonsterIndex) =>
     -- no proof in engagement phase this is correct.... should change player or game from record to data..
     case (skillHead game) of
      Nothing =>
       case serverUpdate of
        DirectAttack =>
         let opponent = getPlayer (getOpponent actor) (playerA game) (playerB game) in
         case Data.Vect.find (\x => actualAlive x) (flattenBoard $ board opponent) of
          Nothing =>
           case (thoughtsResource player > 0) of
            False => Left "Invalid move. Direct attacks consume 1 thought. You have 0 thoughts, and cannot afford this cost."
            True =>
             let player' = record {thoughtsResource $= \x => x - 1} player in
             let fieldedMonster' = Just $ record {basic -> engagement = bind 1} fieldedMonster in
             let board' = unflattenBoard (Vect.replaceAt fieldedMonsterIndex fieldedMonster' (flattenBoard (board player))) in
             let player'' = record {board = board'} player' in
             let x = 56 in
             let opponent' = record {soulCards $= id} opponent {-decrease soul points-} in ?hole {-shoot. I also have to get whether or not to put a skill on the queue...-}

                   -- ?hole -- actually damage enemy soul by 1 for a cost of 1 thought(and consume card turn)
          _ =>  Left "Invalid move. Direct attacks are only possible if there are no living units in the enemy field."
        AttackRow row => ?hole -- don't allow any attack actions if nothing is in range (cannot even waste turn attacking), so just check to see if row is in range
        Rest =>
         let fieldedMonster' = Just $ record {basic -> engagement = bind 1, basic -> hp $= rest} fieldedMonster in
         let board' = unflattenBoard (Vect.replaceAt fieldedMonsterIndex fieldedMonster' (flattenBoard (board player))) in
         let player' = record {board = board'} player in
         Right $ stepGame (mutator player' game, ?hole) -- there is no rest update, so communicate change in stats. -- need function for marshalling stat changes.
        Move fieldIndex =>
         let playerBoard = flattenBoard (board player) in
         case Vect.index fieldIndex playerBoard of
          Nothing =>
-- Don't need to keep track of the side of the board that the unit on move is on in engagement phase, since I already know whose turn it is in game!!!
           let engagedMonsterOnMove = record {basic -> engagement = bind 1} fieldedMonster in
           let movedToBoard = Vect.replaceAt fieldIndex (Just engagedMonsterOnMove) playerBoard in
           let movedFromBoard = Vect.replaceAt fieldedMonsterIndex Nothing movedToBoard in
           Right $ stepGame (mutator (record {board = unflattenBoard movedFromBoard} player) game, [MoveUnit fieldedMonsterIndex fieldIndex actor])
          Just _ => Left "Invalid move. You may not move your card to an occupied square."
        SkillInitiation skillIndex =>
         case (muted getMonsterOnMove) of
          True => Left "Invalid move. Your card cannot initiate action skills its first turn fielded"
          False =>
           case index' skillIndex (actionSkills getMonsterOnMove) of
            Nothing => Left "Invalid move. You appear to not have that action skill."
            -- skillType kind of pointless here....

            Just (MkSkill automatic cost condition) =>
             let MkCost rInteger = cost in
             case getValue rInteger player opponent (MkEnv []) of
              Nothing => ?errorCase
              Just costValue =>
               if (extractBounded $ thoughtsResource player) < costValue
                then
                 Left "Invalid move. You cannot afford the cost to activate this skill."
                else
                 case satisfiedExistentialCondition condition player opponent (MkEnv []) of
                  Nothing => ?errorCase
                  Just False => Left "Invalid move. The conditional for this skill is not satisfied."
                  Just True =>
                   let player' = record {thoughtsResource = thoughtsResource player - costValue} player in
                   case automatic of
                    MkAutomatic skillEffects next =>
                     let (player'', opponent', deathQueue', updates) = applySkillEffects skillEffects player' opponent (id $ basic $ fieldedMonster)(deathQueue game) (MkEnv []) in
                     Right $ stepGame (mutator player'' (opponentMutator opponent' (record {deathQueue = deathQueue'} game)), updates)
                    Universal var condition effects next => ?hole -- execute univeral effects.. remember the game can end..
                     
        _ => Left "Invalid move. It is currently the action phase of your card. Please select a valid action for it."
      Just (Existential selection condition ifSelected ifUnable, evokerId, whichPlayer, env) =>
       case serverUpdate of
        SkillSelection
         friendlyFieldSelection
         enemyFieldSelection
         friendlyHandSelection
         enemyHandSelection
         friendlyGraveyardSelection
         enemyGraveyardSelection
         friendlyBanishedSelection
         enemyBanishedSelection
         =>
         case move_interp
               selection
               condition
               ifSelected
               ifUnable
               evokerId
               actor
               friendlyFieldSelection
               enemyFieldSelection
               friendlyHandSelection
               enemyHandSelection
               friendlyGraveyardSelection
               enemyGraveyardSelection
               friendlyBanishedSelection
               enemyBanishedSelection
               (deathQueue game)
               player
               opponent
               env -- Where is env being updated?
         of
          (Left whichPlayer, clientUpdates) => Center (whichPlayer, clientUpdates)
          (Right (skillHead', skillQueue', deathQueue', cardPlayer', cardOpponent'), clientUpdates) =>
           let cardPlayerMutator = getPlayerMutator whichPlayer cardPlayer' in
           let cardOpponentMutator = getPlayerMutator whichPlayer cardOpponent' in
           let game' = cardPlayerMutator $ cardOpponentMutator $ game in
           case skillHead' of
            Nothing =>
             Right $
             stepGame(
              record
               {skillHead = Nothing,
                skillQueue = skillQueue',
                deathQueue = deathQueue'}
               game',
              clientUpdates)
            Just (Existential selection' condition' ifSelected' ifUnable') =>
             Right $
              (record
                {skillHead = Just (Existential selection' condition' ifSelected' ifUnable', evokerId, whichPlayer, env),
                 skillQueue = skillQueue',
                 deathQueue = deathQueue'}
                game',
               clientUpdates,
               generateClientInstruction whichPlayer "Select targets." "Wait for opponent to select targets.") -- horrible instruction... not matching on skill at all yet.
        _ => Left "Invalid move. Select targets for your current skill."
    RevivalPhase => -- I need to make sure when I enter the revive phase I skip over if nobody can revive.
     case transformRevivalPhase player actor (deathQueue game) serverUpdate of
      Left errorMessage => Left errorMessage
      Right (player', deathQueue', updates) =>
       case (myNot (getInitiative game == playerOnMove game)) || (myNot (canReviveAnything (getPlayer (getOpponent actor) (playerA game) (playerB game)))) of
        True =>
         let phase' = nextPhase (phase game) in
         Right $ stepGame (mutator player' (record {phase = phase', deathQueue = deathQueue'} game), updates)
        False =>
         Right $ stepGame (mutator player' (record {deathQueue = deathQueue'} game), updates)
    DeploymentPhase =>
     case transformDeploymentPhase player serverUpdate of --- Where I am checking if the field is full (if that happens the card gets sent to the graveyard from spawn)
      Left errorMessage => Left errorMessage
      Right (player', updates) =>
       case (getInitiative game == playerOnMove game) {-|| ....-} of
        True =>
         Right $ stepGame (mutator player' game, updates)
        False =>
         let phase' = nextPhase (phase game) in
         Right $ stepGame (mutator player' (record {phase = phase'} game), updates)
    _ =>
     case serverUpdate of
      SkillSelection friendlyField enemyField friendlyHand enemyHand friendlyGraveyard enemyGraveyard friendlyBanished enemyBanished => ?hole
      _ => Left "Invalid move. Select targets for your current skill."
   
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

{- for now, because I have holes everywhere, just assert this is total so we can get the draw phase tested -}
-------------------------------------------------------------------------------
transformGame :
 Game ->
 WhichPlayer ->
 ServerUpdate ->
 Three String (WhichPlayer, List ClientUpdate) (Game, List ClientUpdate, ClientInstruction)
transformGame = assert_total transformGame'

{-with (phase game,serverUpdate)
 | (DrawPhase,DrawCard id)                = ?hole {-(game,[])-} {-Maybe-}
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




{-Also have to make sure it's the player's turn!!-}
 | (SpawnPhase,Skip schools)              = if (dominatesVect maxSchools schools) && (dominatesVect schools (knowledge player)) && (totalDifferenceVect schools (knowledge player) <= extractBounded (thoughts player))
                                             then (updateGame game player, [UpdateSchools schools (temporaryId player) (temporaryId opponent)])
                                             else (game, [InvalidMove (temporaryId player)])
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
-}

-------------------------------------------------------------------------------
transformDrawPhase :
 DrawPhase ->
 WhichPlayer ->
 ServerUpdate ->
 Three String (WhichPlayer, List ClientUpdate) (FullGame, List ClientUpdate, ClientInstruction)

transformDrawPhase = ?hole
-------------------------------------------------------------------------------
transformFullGame :
 FullGame ->
 WhichPlayer ->
 ServerUpdate ->
 Three String (WhichPlayer, List ClientUpdate) (FullGame, List ClientUpdate, ClientInstruction)

transformFullGame gameType whichPlayer serverUpdate with (gameType)
 | MkFullGameDrawPhase drawPhase = transformDrawPhase drawPhase whichPlayer serverUpdate
 | MkFullGameGame game =
   case transformGame game whichPlayer serverUpdate of
    Left errorMessage => Left errorMessage
    Center (winner, updates) => Center (winner, updates)
    Right (game', updates, instruction) => Right (MkFullGameGame game', updates, instruction)
        
        -- if someone wins a round have to handle going to the next round? (Is this handled in main?)
-------------------------------------------------------------------------------

