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

%access public export
%default total

%include C "../Glue/idrisFFI.h"
%link C "../Glue/idrisFFI.o"


init : IO ()
init = foreign FFI_C "init" (IO ())

reader : IO String
reader = foreign FFI_C "reader" (IO String)

writer : String -> IO Unit
writer x = foreign FFI_C "writer" (String -> IO Unit) x



{-might want to not use tokens for temporary ids...-}
{-related: no facility for reconnecting yet.. maybe Ur/Web is giving us the players ID, rather than their tokens.. that would make sense.-}
createNewBattle : List Battle -> String -> String -> List Battle
createNewBattle battles originalPlayerA originalPlayerB = battles ++ [MkBattle FirstRound (new game originalPlayerA originalPlayerB)]



processServerUpdateOnGame : Game -> WhichPlayer -> ServerUpdate -> (Either WhichPlayer Game, List ClientUpdate)
processServerUpdateOnGame = transformGame






{-

record Battle where
 constructor MkBattle
 round : Round
 game : Game


data Round
 = FirstRound
 | SecondRoundOriginalPlayerAWonFirstRound
 | SecondRoundOriginalPlayerBWonFirstRound

-}


data Result
 = Tie
 | OriginalPlayerAWon
 | OriginalPlayerBWon

nextRound : (winner : WhichPlayer) -> Round -> Either Result Round
nextRound PlayerA FirstRound = Right SecondRoundOriginalPlayerAWonFirstRound
nextRound PlayerB FirstRound = Right SecondRoundOriginalPlayerBWonFirstRound
nextRound PlayerA SecondRoundOriginalPlayerAWonFirstRound = Left Tie
nextRound PlayerB SecondRoundOriginalPlayerAWonFirstRound = Left OriginalPlayerAWon
nextRound PlayerA SecondRoundOriginalPlayerBWonFirstRound = Left OriginalPlayerBWon
nextRound PlayerB SecondRoundOriginlaPlayerBWonFirstRound = Left Tie

correctForRound : Round -> WhichPlayer -> WhichPlayer
correctForRound FirstRound PlayerA = PlayerA
correctForRound FirstRound PlayerB = PlayerB
correctForRound _ PlayerA = PlayerB
correctForRound _ PlayerB = PlayerA


replyWith : List ClientUpdate -> String -> String -> String
replyWith clientUpdates playerId opponentId = ?hole {-
 case payload clientUpdates playerId opponentId of
  Just x => x
  Nothing => ?hole {-should payload be able to produce Nothing?-}
-}
{- payload only takes one update :/ -}

{-How do I propagate changes to the round?-}






{-NEED TO ADD updates for going to the next round, and also need to handle the case where the game has ended due to the second round ending....-}


playerIdOpponentId : WhichPlayer -> String -> String -> (String,String)
playerIdOpponentId PlayerA a b = (a,b)
playerIdOpponentId PlayerB a b = (b,a)

processServerUpdate' : Battle -> WhichPlayer -> ServerUpdate -> (List Battle, String)
processServerUpdate' (MkBattle round game) whichPlayer serverUpdate =
 let (transformedGame, clientUpdates) = processServerUpdateOnGame game (correctForRound round whichPlayer) serverUpdate in
  case transformedGame of
   Left winner =>
    case nextRound winner round of
     Left result => ([], ?hole)
     Right round' => ([MkBattle round' (switchSides game)], ?hole)
   Right game' =>
    let (playerId, opponentId) = playerIdOpponentId whichPlayer (getOriginalPlayerTemporaryId PlayerA game) (getOriginalPlayerTemporaryId PlayerB game) in
    ([MkBattle round game'], replyWith clientUpdates playerId opponentId)


processServerUpdate : List Battle -> ServerUpdateWrapper -> (List Battle, String) {-can make the two messages for ur/web delimited with a special character like ~ ... actually can have opponent second.-}
processServerUpdate [] _ = ([],"{updateType: notInAnyGame}") {- what about opponent? Also include playerID???? -}
processServerUpdate ((MkBattle round game)::battles) (MkServerUpdateWrapper serverUpdate playerId) =
 let originalPlayerAId = getOriginalPlayerTemporaryId PlayerA game in
 let originalPlayerBId = getOriginalPlayerTemporaryId PlayerB game in
 case (originalPlayerAId == playerId) of
  True =>
   let (battle', reply) = processServerUpdate' (MkBattle round game) PlayerA serverUpdate in
   (battle' ++ battles, reply)
  False =>
   case (originalPlayerBId == playerId) of
    True =>
     let (battle', reply) = processServerUpdate' (MkBattle round game) PlayerB serverUpdate in
     (battle' ++ battles, reply)
    False => {-DO NOT THROW BATTLES AWAY!!-}processServerUpdate battles (MkServerUpdateWrapper serverUpdate playerId)

{-assuming not the same token for both...-}
 



processMessage : List Battle -> String -> (List Battle, String)
processMessage battles message =
 case parseJson message of
  InvalidRequest =>
   (battles, ?hole) {- should maybe handle the message for this in client updates -}
  NewGameMessage playerId opponentId =>
   (createNewBattle battles playerId opponentId, ?hole) {-similarly probably want a game started and battle started client update-}
  ServerUpdateMessage serverUpdate =>
   processServerUpdate battles serverUpdate


partial
statefulBackend : List Battle -> IO ()
statefulBackend battles = reader >>= (\rawServerMessage => let (battles',clientPayloads) = processMessage battles rawServerMessage in (writer clientPayloads) >>= (\_ => statefulBackend battles'))

partial
main' : IO () {- switch to this when I'm ready... -}
main' = statefulBackend []


partial
mainDummy : IO ()
mainDummy = do {
x <- reader;
writer (x ++ " was received via Idris, the god of languages. This game is ready to be built in god mode!");
mainDummy;
}

partial
main : IO ()
main = do {
_ <- init;
main';
}

{-units now should become engaged AFTER their skills finish (if that's not too hard) actually that might be too hard...-}

