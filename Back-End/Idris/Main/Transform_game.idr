module Main.Transform_game
--import Control.ST
import Data.So
import Data.Vect
import Data.Fin
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

{-
partial
statefulTransformGame : (game : Game) -> (actor : WhichPlayer) -> (serverUpdate : ServerUpdate) -> STrans m (Either WhichPlayer Game, List ClientUpdate) [] (const [])
statefulTransformGame init actor serverUpdate = do
 game <- new init
 updates <- new (the (List ClientUpdate) [])
 
 if (playerOnMove init == actor)
  then
   let (player, mutator) = getStatefulPlayer actor init in
   case phase init of
    SpawnPhase =>
     case transformSpawnPhase player serverUpdate of
      Left errorMessage => write game init
      Right (player', updates) =>
       case (getInitiative init == playerOnMove init) of
        True =>
         let (game', updates', instruction) = stepGame (mutator player' init, []) in write game init
        False => write game init 
                 --write game init
  else do
   write updates [InvalidMove Clientupdates.notYourTurn actor]
 write game init

 resultGame <- read game
 resultUpdates <- read updates
 let result = (Right resultGame, resultUpdates)
 delete game
 delete updates
 pure result
 

test : Game -> (Either WhichPlayer Game, List ClientUpdate)
test game = runPure (statefulTransformGame game PlayerA Rest)
-}

{-

generateClientInstruction :
  WhichPlayer ->
    String ->
      String ->
        ClientInstruction
         generateClientInstruction whichPlayerOnMove onMoveMessage notOnMoveMessage
         -}


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

-- I can make a nice monad for this...
-- NEED TO STEP GAME TOO?

transformGame' game actor serverUpdate =
 case (playerOnMove game == actor) of
  False => Left Clientupdates.notYourTurn
  True =>
   let (player, mutator) = getStatefulPlayer actor game in
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
    EngagementPhase (fieldedMonster, fieldedMonsterIndex) => -- no proof in engagement phase this is correct.... should change player or game from record to data..
     case (skillHead game) of



           -- THIS IS ONE BIG THING

      TerminatedSkill =>
       case serverUpdate of
        DirectAttack =>
         let opponent = getPlayer (getOpponent actor) (playerA game) (playerB game) in
         case Data.Vect.find (\x => actualAlive x) (flattenBoard $ board opponent) of
          Nothing =>
           case (thoughtsResource player > 0) of
            False => Left "Invalid move. Direct attacks consume 1 thought. You have 0 thoughts, and cannot afford this cost."
            True =>
             let player' = record {thoughtsResource $= \x => x - 1} player in
             let player'' = record {board $= ?hole {-engage unit that is direct attacking!!-}} player' in
             let (opponent, opponentMutator) = getStatefulPlayer (getOpponent actor) game in
             let x = 56 in
             let opponent' = {-record {soulCards = soulCards opponent}-} opponent {-decrease soul points-} in ?hole {-shoot. I also have to get whether or not to put a skill on the queue...-}

                   -- ?hole -- actually damage enemy soul by 1 for a cost of 1 thought(and consume card turn)
          _ =>  Left "Invalid move. Direct attacks are only possible if there are no living units in the enemy field."
        AttackRow row => ?hole -- don't allow any attack actions if nothing is in range (cannot even waste turn attacking), so just check to see if row is in range
        Rest => ?hole -- this is always valid
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

            Just (MkSkill automatic cost condition skillType) =>
             let MkCost rInteger = cost in
             case getValue rInteger ?player ?opponent ?env of
              Nothing => ?errorCase
              Just costValue =>
               if (extractBounded $ thoughtsResource player) < costValue
                then
                 Left "Invalid move. You cannot afford the cost to activate this skill."
                else
                 case satisfiedExistentialCondition condition ?player ?opponent ?emptyEnv of
                  Nothing => ?errorCase
                  Just False => Left "Invalid move. The conditional for this skill is not satisfied."
                  Just True =>
                   case automatic of
                    MkAutomatic skillEffects next evokerId playerId =>
                     let x = applySkillEffects skillEffects ?player ?opponent ?env in -- what do I do with the evoker here? should also pass in right????
                     ?hole
                    Universal var condition effects next evokerId playerId => ?hole

                        
                        
                        ----?hole 
                        {-
                  
                  $ stepGame (pushSkill automatic (mutator (record {thoughtsResource = thoughtsResource player - costValue} player) game), ?hole)
             
-}


{-

data Automatic
     = MkAutomatic (List SkillEffect) Nonautomatic Nat String 
        | Universal (String,Set) Condition (List SkillEffect) Nonautomatic Nat String

 List SkillEffect ->
  Player ->
   Player ->
    Env ->
     (Player,Player,List ClientUpdate) --- I also need the death queue to be updated, right!?!?
     -}


                              --?deductCostFromThoughtsAndLoadSkill

                 {-

                 satisfiedExistentialCondition :
                  Condition ->
                   Player ->
                    Player ->
                     Env ->
                      Maybe Bool

                      -}
             
{-
getValue :
 RInteger ->
  Player ->
   Player ->
    Env ->
     Maybe Integer-}


        _ => Left "Invalid move. It is currently the action phase of your card. Please select a valid action for it."
      Existential selection condition ifSelected ifUnable cardId playerId =>
       case serverUpdate of

            -- THIS IS ONE BIG THING

        SkillSelection friendlyFieldSelection enemyFieldSelection friendlyHandSelection enemyHandSelection friendlyGraveyardSelection enemyGraveyardSelection =>
         --  Either (Either Player Player) (Nonautomatic, List Skill, List Nat, Player, Player)
         case move_interp selection condition ifSelected ifUnable cardId playerId ?friendlyFieldSelection ?enemyFieldSelection friendlyHandSelection enemyHandSelection friendlyGraveyardSelection enemyGraveyardSelection ?friendlyBanishedSelection ?enemyBanishedSelection ?deathQueue ?playerHole ?playerHole ?envHole of
          (Left (Left winningPlayerBlarg), clientUpdates) => ?hole
          (Left (Right winningPlayerOtherBlarg), clientUpdates) => ?hole
          (Right (skillHead', skillQueue', deathQueue', somePlayer, someOtherPlayer), clientUpdates) =>
            case skillHead' of
             TerminatedSkill => Right $ stepGame (record {skillHead = skillHead', skillQueue = skillQueue', deathQueue = deathQueue', playerA = ?hole, playerB = ?hole} game, clientUpdates)
             Existential argsE conditionE selectedE failedE cardIdE playerIdE =>
              let whichPlayer = ?hole in -- really want playerId to be a WhichPlayer, not an actually Id here...
              Right $ (record {skillHead = skillHead', skillQueue = skillQueue', deathQueue = deathQueue', playerA = ?hole, playerB = ?hole} game,
               clientUpdates,
               generateClientInstruction whichPlayer "Select targets." "Wait for opponent to select targets.") -- horrible instruction... not matching on skill at all yet.

---- Three String (WhichPlayer, List ClientUpdate) (Game, List ClientUpdate, ClientInstruction) -- user error, winning player or the game



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
      SkillSelection friendlyField enemyField friendlyHand enemyHand friendlyGraveyard enemyGraveyard => ?hole
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

{- 
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
transformGame :
 Game ->
 WhichPlayer ->
 ServerUpdate ->
 Three String (WhichPlayer, List ClientUpdate) (Game, List ClientUpdate, ClientInstruction)
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
--Three String (WhichPlayer, List ClientUpdate) (Game, List ClientUpdate, ClientInstruction)


