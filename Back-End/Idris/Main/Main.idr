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
getPlayerByTemporaryId playerTemporaryId game = if (temporaryId (player_A game)) == playerTemporaryId then Just (player_A game) else if (temporaryId (player_B game)) == playerTemporaryId then Just (player_B game) else Nothing

createNewGame : List Game -> String -> String -> List Game
createNewGame games originalPlayerA originalPlayerB = games ++ [new game originalPlayerA originalPlayerB]


{-
processServerUpdateOnGame : Game -> Player -> Player -> WhichPlayer -> ServerUpdate -> (Game, List ClientUpdate)
processServerUpdateOnGame = transformGame
-}

processServerUpdate : List Game -> ServerUpdateWrapper -> (List Game, String) {-can make the two messages for ur/web delimited with a special character like ~ ... actually can have opponent second.-}
processServerUpdate [] _ = ([],"{updateType: notInAnyGame}") {- what about opponent? Also include playerID???? -}
processServerUpdate (game::games) (MkServerUpdateWrapper serverUpdate playerId) = let x = transformGame ?hole ?hole ?hole in ?hole


{-Game -> WhichPlayer -> ServerUpdate -> (Game, List ClientUpdate)-}

{- THE ABOVE IS JUST TO TRIGGER TRANSFORM GAME TO BE TYPECHECKED. STILL NEED CORRECT CODE FOR TRAVERSING THIS. -}




processMessage : List Game -> String -> (List Game, String)
processMessage games message =
  case parseJson message of
       InvalidRequest => (games, ?hole) {- should maybe handle the message for this in client updates -}
       NewGameMessage playerId opponentId => (createNewGame games playerId opponentId, ?hole) {-similarly probably want a game started client update-}
       ServerUpdateMessage serverUpdate => processServerUpdate games serverUpdate


partial
statefulBackend : List Game -> IO ()
statefulBackend games = reader >>= (\rawServerMessage => let (games',clientPayloads) = processMessage games rawServerMessage in (writer clientPayloads) >>= (\_ => statefulBackend games'))

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

