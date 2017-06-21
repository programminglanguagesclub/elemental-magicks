module Main.Spawn_phase
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
getPlayer : WhichPlayer -> Player -> Player -> Player
getPlayer PlayerA playerA playerB = playerA
getPlayer PlayerB playerA playerB = playerB
getOpponent : WhichPlayer -> Player -> Player -> Player
getOpponent PlayerA playerA playerB = playerB
getOpponent PlayerB playerA playerB = playerA

youSpawn : String
opponentSpawns : String

youSpawn = "Level your schools of thought as desired, and either select a card from your hand to spawn, or skip."
opponentSpawns = "Wait for your opponent to level their schools of thought, and decide whether or not to spawn a card."

{-getMessageSpawnPhase : WhichPlayer -> Player -> Player -> ClientInstruction-}

stepSpawnPhase : WhichPlayer -> Player -> Player -> Maybe ClientInstruction
stepSpawnPhase PlayerA playerA playerB with (spawnCard playerA)
 | Nothing = Just $ MkClientInstruction (youSpawn, opponentSpawns)
 | Just _ = case spawnCard playerB of
                 Nothing => Just $ MkClientInstruction (opponentSpawns, youSpawn)
                 Just _ => Nothing
stepSpawnPhase PlayerB playerA playerB with (spawnCard playerB)
 | Nothing = Just $ MkClientInstruction (opponentSpawns, youSpawn)
 | Just _ = case (spawnCard playerA) of
                 Nothing => Just $ MkClientInstruction (youSpawn, opponentSpawns)
                 Just _ => Nothing

{-I can remove some boilerplate by having a function that generates client instructions given who has the initiative,
and the message for the player with and without the initiative-}




{-
 let player = getPlayer initiative playerA playerB in
 case (spawnCard player) of

{-This is all wrong, since I'm giving the first instruction to player A when player A might not be the one to spawn... now.......-}

      Nothing => Just $ MkClientInstruction (youSpawn,opponentSpawns)
      Just _ => case (spawnCard $ getOpponent initiative playerA playerB) of
                     Nothing => Just $ MkClientInstruction (opponentSpawns,youSpawn)
-}



{-

 | SpawnPhase = case transformSpawnPhase actor (player_A game) (player_B game) (initiative game) serverUpdate of
                     Right (errorMessage, playerId) => ?hole
                     Left ((playerA', playerB'), updates) => ?hole


-}

transformSpawnPhase : WhichPlayer -> Player -> Player -> WhichPlayer -> ServerUpdate -> Either (String, String) ((Player,Player),List ClientUpdate)








