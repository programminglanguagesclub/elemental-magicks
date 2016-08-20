module Main

import Prelude.Nat
import Data.Vect
import Data.Fin
import Data.So
import preliminaries
import phase
import objects_basic
import skill_dsl
import player
import game
import serverupdates
import clientupdates
import transform_game
import transform_game_helpers
import step_game
import step_game_helpers


%include C "../Glue/reader.h"
%link C "../Glue/reader.o"

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




main : IO ()
main = do {
x <- reader;
writer (x ++ " was received via Idris, the god of languages. This game is ready to be built in god mode!");
main;
}



{-units now should become engaged AFTER their skills finish (if that's not too hard) actually that might be too hard...-}



