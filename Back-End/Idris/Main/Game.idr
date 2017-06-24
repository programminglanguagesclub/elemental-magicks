module Main.Game
import Data.Fin
import Data.Vect
import Base.BoundedList
import Base.Skill_dsl_data
import Base.Player
import Base.Phase
import Base.Card
import Base.Clientupdates

%access public export
%default total

-------------------------------------------------------------------------------
data WhichPlayer
 = PlayerA
 | PlayerB
-------------------------------------------------------------------------------
getOpponent : WhichPlayer -> WhichPlayer
getOpponent PlayerA = PlayerB
getOpponent PlayerB = PlayerA
-------------------------------------------------------------------------------
data Round
 = FirstRound
 | SecondRoundOriginalPlayerAWonFirstRound
 | SecondRoundOriginalPlayerBWonFirstRound
-------------------------------------------------------------------------------
{-Reset used_death_skill, used_counter_skill before auto skill and action of card. -}

-------------------------------------------------------------------------------
record Game where
 constructor MkGame
 initiative : WhichPlayer
 turnNumber : Nat
 skillHead : Nonautomatic
 skillQueue : List Automatic
 deathQueue : List Nat {-The temporary ids of the monster (maybe this should have its own type?)-}
 phase : Phase
-------------------------------------------------------------------------------
newGame :
 String ->
 String ->
 Game

newGame playerAId playerBId =
 let playerA = newDrawPlayer playerAId in
 let playerB = newDrawPlayer playerBId in
 MkGame PlayerA 0 TerminatedSkill [] [] (DrawPhase playerA playerB 0)
-------------------------------------------------------------------------------

switchSides : Game -> Game
switchSides game with (phase game)
 | (DrawPhase _ _ _) = ?hole {-error case!!-}
 | (MkPhaseCycle _ playerA playerB) = newGame (temporaryId playerB) (temporaryId playerA)

{-
{-this is another case where I would like to drill into game having already pattern matched away the possibility that I can't access phase the way I want (pattern matching...)-}
switchSides (MkGame _ _ _ _ _ (MkPhaseCycle _ playerA playerB)) = new game (temporaryId playerB) (temporaryId playerA)
-}

playerOnMove : Game -> WhichPlayer

-------------------------------------------------------------------------------
record Battle where
 constructor MkBattle
 round : Round
 game : Game
-------------------------------------------------------------------------------
getPlayerTemporaryId :
 WhichPlayer ->
 Game ->
 String

getPlayerTemporaryId whichPlayer game with (phase game)
 | DrawPhase playerA playerB _ =
  case whichPlayer of
   PlayerA => temporaryId playerA
   PlayerB => temporaryId playerB
 | MkPhaseCycle _ playerA playerB =
  case whichPlayer of
   PlayerA => temporaryId playerA
   PlayerB => temporaryId playerB
-------------------------------------------------------------------------------
{-
updatePlayer : Game -> WhichPlayer -> (Player -> (Player,List ClientUpdate)) -> (Game, List ClientUpdate)
updatePlayer game PlayerA f = let (playerA',updates) = f $ player_A game in
                                  (record {player_A = playerA'} game, updates)
updatePlayer game PlayerB f = let (playerB',updates) = f $ player_B game in
                                  (record {player_B = playerB'} game, updates)

-}

