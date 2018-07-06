module Main.Spawn_phase
import Data.Vect
import Base.Bounded
import Base.Preliminaries
import Base.Player
import Base.Card
import Main.Game
import Base.Clientupdates
import Main.Serverupdates
%access public export
%default total



{-
yourHand : String
yourHand = "Select a card to add to your hand."
opponentHand : String
opponentHand = "Wait for your opponent to add a card to their hand."
yourSoul : String
yourSoul = "Select a position in your soul and a card to add to your soul."
opponentSoul : String
opponentSoul = "Wait for your opponent to add a card to their soul."



serializeSequence : CardDraw -> WhichPlayer -> String
serializeSequence HA PlayerA = yourHand
serializeSequence HA PlayerB = opponentHand
serializeSequence SA PlayerA = yourSoul
serializeSequence SA PlayerB = opponentSoul
serializeSequence HB PlayerA = opponentHand
serializeSequence HB PlayerB = yourHand
serializeSequence SB PlayerA = opponentSoul
serializeSequence SB PlayerB = yourSoul


-}


{-players with and without initiative-}
{-Should this be here????-}
getPlayer : WhichPlayer -> Player -> Player -> Player
getPlayer PlayerA playerA playerB = playerA
getPlayer PlayerB playerA playerB = playerB
getOpponent : WhichPlayer -> Player -> Player -> Player
getOpponent PlayerA playerA playerB = playerB
getOpponent PlayerB playerA playerB = playerA

-------------------------------------------------------------------------------
youSpawn : String
opponentSpawns : String

youSpawn =
 "Level your schools of thought as desired, and either select a card from your hand to spawn, or skip."
opponentSpawns =
 "Wait for your opponent to level their schools of thought, and decide whether or not to spawn a card."
-------------------------------------------------------------------------------
stepSpawnPhase :
 WhichPlayer ->
 Player ->
 Player ->
 Maybe ClientInstruction

stepSpawnPhase initiative playerA playerB with (spawnCard (getPlayer initiative playerA playerB))
 | Nothing =
  Just $
  generateClientInstruction initiative youSpawn opponentSpawns
 | Just _ =
  let onMove = getOpponent initiative in
  case spawnCard $ getPlayer onMove playerA playerB of
   Nothing =>
    Just $ generateClientInstruction onMove youSpawn opponentSpawns
   Just _ => Nothing
-------------------------------------------------------------------------------
transformSpawnPhase :
 (actor : WhichPlayer) ->
 (playerA : Player) ->
 (playerB : Player) ->
 (whichPlayerOnMove : WhichPlayer) -> -- THIS IS CALLED WITH INITIATIVE??? IF SO THAT MIGHT BE WRONG...
 (serverUpdate : ServerUpdate) ->
 Either
  (String, String)
  ((Player,Player),List ClientUpdate)

transformSpawnPhase actor a b whichPlayerOnMove update =
 case (whichPlayerOnMove == actor) of
  False => Left (notYourTurn, temporaryId $ getPlayer actor a b)
  True =>
   let playerToUpdate = getPlayer whichPlayerOnMove a b in
   case update of
    SpawnCard knowledge' handIndex =>
     case dominatesVect knowledge' (knowledge playerToUpdate) of
      True =>
       case index' handIndex (hand playerToUpdate) of
        Nothing => Left ("You selected a position in your hand that does not contain a card", ?hole)
        Just (SpellCard spell) => ?hole -- shouldn't cards in the hand not have a permanent or temporary stat????
        Just (MonsterCard monster) => ?hole
      False => Left ("You cannot lower your knowledge in the spawn phase!", ?hole)
    Skip knowledge' =>
     case dominatesVect knowledge' (knowledge playerToUpdate) of
      True =>
       let cost = totalDifferenceVect knowledge' (knowledge playerToUpdate) in
       let currentThoughts = extractBounded $ thoughtsResource playerToUpdate {-not always playerA-} in
       case currentThoughts >= cost of
        True =>
         Right
          ((record {thoughtsResource $= (\x => x - cost), knowledge = knowledge'} playerToUpdate,
          getOpponent playerToUpdate), ?hole)
        False => Left ("You cannot afford to raise your knowledge by that much!",?hole)
      False => Left("You cannot lower your knowledge in the spawn phase!", ?hole)
    _ => Left ("You can only play cards or skip in the spawn phase",{-temporaryId someplayer-} ?hole) -- can only play cards in spawn.
-------------------------------------------------------------------------------



