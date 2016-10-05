module Main.Main

import Prelude.Nat
import Data.Vect
import Data.Fin
import Data.So
import Base.Preliminaries
import Base.Phase
import Base.Objects_basic
import Base.Player
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
import Main.Transform_game
import Main.Transform_game_helpers
import Main.Step_game
import Main.Step_game_helpers

%access public export
%default total

%include C "../Glue/idrisFFI.h"
%link C "../Glue/idrisFFI.o"

reader : IO String
reader = foreign FFI_C "reader" (IO String)

writer : String -> IO Unit
writer x = foreign FFI_C "writer" (String -> IO Unit) x



getPlayerByTemporaryId : String -> Game -> Maybe Player
getPlayerByTemporaryId playerTemporaryId game = if (temporaryId (player_A game)) == playerTemporaryId then Just (player_A game) else if (temporaryId (player_B game)) == playerTemporaryId then Just (player_B game) else Nothing


{-
while_loop : List Game -> ServerUpdateWrapper -> (List Game, List ClientUpdate) {-ClientUpdate or ClientUpdateWrapper?-}
while_loop [] _      = ?hole {-([],[])-}
while_loop (g::gs) (playerTemporaryId, serverUpdate) with (getPlayerByTemporaryId playerTemporaryId g)
 | Nothing = let (gs',cus) = while_loop gs (playerTemporaryId, serverUpdate) in (g::gs',cus)
 | Just player = let (g',cus) = transformGame g player serverUpdate in (g'::gs, cus)


Need to get opponent to call transformGame

-}

{- TOKEN WRONG = if (temporaryId (player_A g)) == player_token || (temporaryId (player_B g)) == player_token then let (g',cus) = transformGame g (player_token, serverUpdate) in (g'::gs, cus)
                                                  else let (gs',cus) = while_loop gs (player_token, serverUpdate) in (g::gs',cus) -}



{-I also need a mechanism for creating games. Ur/Web can handle this and just tell Idris when it has happened...-}



createNewGame : List Game -> String -> String -> List Game
createNewGame games originalPlayerA originalPlayerB = ?hole {-this actually can be a special case of processserverupdate....??-}


processServerUpdate : List Game -> ServerUpdate -> (List Game, String) {-can make the two messages for ur/web delimited with a special character like ~ ... actually can have opponent second.-}
{-processServerUpdate games serverUpdate = ?hole {- find game in list of games, then transformGame -}-}
processServerUpdate [] _ = ([],?hole) {- need a client update for not in any game -}
processServerUpdate (game::games) serverUpdate = ?hole {- create function for getting player id out of server update? Perhaps should be server update wrapper and not server update? -}




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
main : IO ()
main = do {
x <- reader;
writer (x ++ " was received via Idris, the god of languages. This game is ready to be built in god mode!");
main;
}

{-
just trying...


foo : IO ()
foo = reader >>= (\x =>
      writer x >>= (\_ =>
      foo))

-}







{-units now should become engaged AFTER their skills finish (if that's not too hard) actually that might be too hard...-}



