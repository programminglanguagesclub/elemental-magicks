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
stepSpawnPhase : WhichPlayer -> Player -> Player -> Maybe ClientInstruction
stepSpawnPhase initiative playerA playerB with (spawnCard (getPlayer initiative playerA playerB))
 | Nothing = Just $ generateClientInstruction initiative youSpawn opponentSpawns
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
 (initiative : WhichPlayer) ->
 (serverUpdate : ServerUpdate) ->
 Either
  (String, String)
  ((Player,Player),List ClientUpdate)

-- hand index should really be a Fin or w/e, not a nat.....

transformSpawnPhase actor a b initiative update =
 let whichPlayerOnMove = PlayerA in
 case (whichPlayerOnMove == actor) of
  False => Left (notYourTurn, ?hole{-the player id-}) -- Not Correct Player On Move!!
  True =>
   case update of
    SpawnCard knowledge handIndex => ?hole
    Skip knowledge => ?hole
    _ => Left ("You can only play cards or skip in the spawn phase",{-temporaryId someplayer-} ?hole) -- can only play cards in spawn.
-------------------------------------------------------------------------------



