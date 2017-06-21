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


getPlayerByTemporaryId : String -> Game -> Maybe Player
getPlayerByTemporaryId = ?hole {-playerTemporaryId game = if (temporaryId (player_A game)) == playerTemporaryId then Just (player_A game) else if (temporaryId (player_B game)) == playerTemporaryId then Just (player_B game) else Nothing
-}



{-might want to not use tokens for temporary ids...-}
createNewBattle : List Battle -> String -> String -> List Battle
createNewBattle battles originalPlayerA originalPlayerB = battles ++ ?hole {-[new game originalPlayerA originalPlayerB]-}



processServerUpdateOnGame : Game -> WhichPlayer -> ServerUpdate -> (Game, List ClientUpdate)
processServerUpdateOnGame = transformGame




-- transformGame : Game -> WhichPlayer -> ServerUpdate -> (Game, List ClientUpdate)


{-

record Battle where
 constructor MkBattle
 round : Round
 originalPlayerAToken : String
 originalPlayerBToken : String
 game : Game

-}

correctForRound : Round -> WhichPlayer -> WhichPlayer
correctForRound FirstRound PlayerA = PlayerA
correctForRound FirstRound PlayerB = PlayerB
correctForRound _ PlayerA = PlayerB
correctForRound _ PlayerB = PlayerA


{-How do I propagate changes to the round?-}

processServerUpdate : List Battle -> ServerUpdateWrapper -> (List Battle, String) {-can make the two messages for ur/web delimited with a special character like ~ ... actually can have opponent second.-}
processServerUpdate [] _ = ([],"{updateType: notInAnyGame}") {- what about opponent? Also include playerID???? -}
processServerUpdate ((MkBattle round originalPlayerAToken originalPlayerBToken game)::battles) (MkServerUpdateWrapper serverUpdate playerId) =
 case (originalPlayerAToken == playerId) of
  True =>
   let (game', clientUpdates) = processServerUpdateOnGame game (correctForRound round PlayerA) serverUpdate in
   ((MkBattle ?hole originalPlayerAToken originalPlayerBToken game')::battles, ?hole)
  False =>
   case (originalPlayerBToken == playerId) of
    True =>
     let (game', clientUpdates) = processServerUpdateOnGame game (correctForRound round PlayerB) serverUpdate in
     ((MkBattle ?hole originalPlayerAToken originalPlayerBToken game')::battles, ?hole)
    False => processServerUpdate battles (MkServerUpdateWrapper serverUpdate playerId)

{-assuming not the same token for both...-}
 
 {- let x = transformGame ?hole serverUpdate in
 ?hole
-}

{-Game -> WhichPlayer -> ServerUpdate -> (Game, List ClientUpdate)-}

{- THE ABOVE IS JUST TO TRIGGER TRANSFORM GAME TO BE TYPECHECKED. STILL NEED CORRECT CODE FOR TRAVERSING THIS. -}




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
mainHelper : IO ()
mainHelper = do {
x <- reader;
writer (x ++ " was received via Idris, the god of languages. This game is ready to be built in god mode!");
mainHelper;
}

partial
main : IO ()
main = do {
_ <- init;
mainHelper;
}

{-units now should become engaged AFTER their skills finish (if that's not too hard) actually that might be too hard...-}

