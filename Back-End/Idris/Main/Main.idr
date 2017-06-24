module Main.Main

import Data.Vect
import Data.So
import Base.Bounded
import Base.BoundedList
import Base.Preliminaries
import Base.Player
import Base.Card
import Main.Serverupdates
import Main.Game
import Main.Transform_game
import Base.Skill_dsl_data
import Base.Phase
import Base.Clientupdates
import Effects
import Effect.Select


%access public export
%default total

%include C "../Glue/idrisFFI.h"
%link C "../Glue/idrisFFI.o"
-------------------------------------------------------------------------------
init : IO ()

init = foreign FFI_C "init" (IO ())
-------------------------------------------------------------------------------
reader : IO String

reader = foreign FFI_C "reader" (IO String)
-------------------------------------------------------------------------------
writer : String -> IO Unit

writer x = foreign FFI_C "writer" (String -> IO Unit) x
-------------------------------------------------------------------------------
--Idris supports random numbers, but using C anyway
--Unimplemented
getRandom : IO Int

getRandom = foreign FFI_C "getRandom" (IO Int)
-------------------------------------------------------------------------------
{-processServerUpdateOnGame :
 Game ->
 WhichPlayer ->
 ServerUpdate ->
 (Either WhichPlayer Game, List ClientUpdate)

processServerUpdateOnGame = transformGame-}
-------------------------------------------------------------------------------
nextRound :
 (winner : WhichPlayer) ->
 Round ->
 Either Result Round

nextRound PlayerA FirstRound = Right SecondRoundOriginalPlayerAWonFirstRound
nextRound PlayerB FirstRound = Right SecondRoundOriginalPlayerBWonFirstRound
nextRound PlayerA SecondRoundOriginalPlayerAWonFirstRound = Left Tie
nextRound PlayerB SecondRoundOriginalPlayerAWonFirstRound = Left OriginalPlayerAWon
nextRound PlayerA SecondRoundOriginalPlayerBWonFirstRound = Left OriginalPlayerBWon
nextRound PlayerB SecondRoundOriginlaPlayerBWonFirstRound = Left Tie
-------------------------------------------------------------------------------
correctForRound :
 Round ->
 WhichPlayer ->
 WhichPlayer

correctForRound FirstRound PlayerA = PlayerA
correctForRound FirstRound PlayerB = PlayerB
correctForRound _ PlayerA = PlayerB
correctForRound _ PlayerB = PlayerA
-------------------------------------------------------------------------------
{-
 unless I can prove that ClientUpdates never produce errors when run with payload,
 I should traverse the list of client updates and return an internal server error
 if payload of any of them produces Nothing.
-}
-------------------------------------------------------------------------------
trivial : (String -> String -> String) -> Maybe String -> Maybe String -> Maybe String
--trivial f a b = f <$> a <*> b -- THIS IS CAUSING COMPILE ISSUE WITH IMPLICITS
-------------------------------------------------------------------------------
replyWith' : List ClientUpdate -> String -> String -> Maybe String

replyWith' [] playerId opponentId = Just "" -- this is probably an error case.
replyWith' [x] playerId opponentId = payload x playerId opponentId
replyWith' (x1::x2::xs) playerId opponentId =
 let firstPayload = ((++) "|") <$> (payload x1 playerId opponentId) in
 trivial (++) firstPayload (replyWith' (x2::xs) playerId opponentId)
-------------------------------------------------------------------------------
replyWith : List ClientUpdate -> String -> String -> String
replyWith clientUpdates playerId opponentId =
 case replyWith' clientUpdates playerId opponentId of
  Nothing => ?hole -- can probably make this impossible (by making updates have playerIds in their type?)
  Just serverResponse => serverResponse
-------------------------------------------------------------------------------
playerIdOpponentId :
 WhichPlayer ->
 String ->
 String ->
 (String,String)

playerIdOpponentId PlayerA a b = (a,b)
playerIdOpponentId PlayerB a b = (b,a)
-------------------------------------------------------------------------------
processServerUpdate' :
 Battle ->
 WhichPlayer ->
 ServerUpdate ->
 (List Battle, String)

processServerUpdate' (MkBattle round game) whichPlayer serverUpdate =
 let aId = getPlayerTemporaryId PlayerA game in
 let bId = getPlayerTemporaryId PlayerB game in
 let (playerId, opponentId) = playerIdOpponentId whichPlayer aId bId in
 let whichPlayer' = correctForRound round whichPlayer in
 let transformGameResult = transformGame game whichPlayer' serverUpdate in
 let (transformedGame, clientUpdates) = transformGameResult in
  case transformedGame of
   Left winner =>
    let winnerId = getPlayerTemporaryId winner game in
    case nextRound winner round of
     Left result =>
      let matchOver = [GameTerminated winnerId, MatchTerminated result] in
      let clientUpdates' = clientUpdates ++ matchOver in
      let serverResponse = replyWith clientUpdates' playerId opponentId in
      ([], serverResponse)
     Right round' =>
      let beginSecondRound = [GameTerminated winnerId, GameStart] in
      let clientUpdates' = clientUpdates ++ beginSecondRound in
      let serverResponse = replyWith clientUpdates' playerId opponentId in
      ([MkBattle round' (switchSides game)], serverResponse)
   Right game' =>
    ([MkBattle round game'], replyWith clientUpdates playerId opponentId)
-------------------------------------------------------------------------------
processServerUpdate :
 List Battle ->
 ServerUpdateWrapper ->
 (List Battle, String)

{-can make the two messages for ur/web delimited with a special character like ~ ... actually can have opponent second.-}
processServerUpdate [] _ = ([],"{updateType: notInAnyGame}") {- what about opponent? Also include playerID???? -}
processServerUpdate ((MkBattle round game)::battles) (MkServerUpdateWrapper serverUpdate playerId) =
 let playerAId = getPlayerTemporaryId PlayerA game in
 let playerBId = getPlayerTemporaryId PlayerB game in
 case (playerAId == playerId) of
  True =>
   let (battle', reply) = processServerUpdate' (MkBattle round game) PlayerA serverUpdate in
   (battle' ++ battles, reply)
  False =>
   case (playerBId == playerId) of
    True =>
     let (battle', reply) = processServerUpdate' (MkBattle round game) PlayerB serverUpdate in
     (battle' ++ battles, reply)
    False =>
     let (battles', reply) = processServerUpdate battles (MkServerUpdateWrapper serverUpdate playerId) in
     ((MkBattle round game)::battles', reply)

{-assuming not the same token for both players...-}
-------------------------------------------------------------------------------
randomlyDecidePlayer : IO WhichPlayer

randomlyDecidePlayer =
 do{
  x <- getRandom;
  pure (if x == 0 then PlayerA else PlayerB);
 }
-------------------------------------------------------------------------------
createBattle :
 String ->
 String ->
 WhichPlayer ->
 Battle

createBattle playerId opponentId PlayerA =
 MkBattle FirstRound (new game playerId opponentId)
createBattle playerId opponentId PlayerB =
 MkBattle FirstRound (new game opponentId playerId)
-------------------------------------------------------------------------------
createBattleMessage :
 String ->
 String ->
 WhichPlayer ->
 String

createBattleMessage playerId opponentId PlayerA =
 replyWith [MatchStart playerId opponentId, GameStart] playerId opponentId
createBattleMessage playerId opponentId PlayerB =
 replyWith [MatchStart opponentId playerId, GameStart] playerId opponentId
-------------------------------------------------------------------------------
addBattle :
 String ->
 String ->
 WhichPlayer ->
 List Battle ->
 (List Battle, String)

addBattle playerId opponentId whichPlayer battles =
 let battle = createBattle playerId opponentId whichPlayer in
 let message = createBattleMessage playerId opponentId whichPlayer in
 (battles ++ [battle], message)
-------------------------------------------------------------------------------
processMessage :
 List Battle ->
 String ->
 IO (List Battle, String)

processMessage battles message =
 case parseJson message of
  InvalidRequest =>
   pure (battles, ?hole) {- should maybe handle the message for this in client updates -}
  NewGameMessage playerId opponentId =>
   randomlyDecidePlayer >>= \whichPlayer =>
   pure $ addBattle playerId opponentId whichPlayer battles
{-this currently does not send any instruction,such as "draw card".I need to make sure instructions are sent everywhere-}
  ServerUpdateMessage serverUpdate =>
   pure $ processServerUpdate battles serverUpdate
-------------------------------------------------------------------------------
partial
statefulBackend : List Battle -> IO ()

statefulBackend battles =
 reader >>= \rawServerMessage =>
 processMessage battles rawServerMessage >>= \(battles', clientPayloads) => 
 writer clientPayloads >>= \_ =>
 statefulBackend battles'
-------------------------------------------------------------------------------
partial
mainDummy : IO ()

mainDummy = do {
 x <- reader;
 writer (x ++ " was received via Idris, the god of languages. This game is ready to be built in god mode!");
 mainDummy;
}
-------------------------------------------------------------------------------
partial
main : IO ()

main = do {
 _ <- init;
 statefulBackend [];
}
-------------------------------------------------------------------------------





{-units now should become engaged AFTER their skills finish (if that's not too hard) actually that might be too hard...-}

