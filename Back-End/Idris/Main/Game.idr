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

data WhichPlayer
 = PlayerA
 | PlayerB

data Round
 = FirstRound
 | SecondRoundOriginalPlayerAWonFirstRound
 | SecondRoundOriginalPlayerBWonFirstRound

{-Reset used_death_skill, used_counter_skill before auto skill and action of card. -}


record Game where
 constructor MkGame
 initiative : WhichPlayer
 turnNumber : Nat
 skillHead : Nonautomatic
 skillQueue : List Automatic
 deathQueue : List Nat {-The temporary ids of the monster (maybe this should have its own type?)-}
 phase : Phase
 

syntax "new" "game" [playerAId] [playerBId] =
 MkGame PlayerA 0 TerminatedSkill [] [] (DrawPhase (MkDrawPlayer [] (replicate 5 Nothing) playerAId) (MkDrawPlayer [] (replicate 5 Nothing) playerBId) 0)


switchSides : Game -> Game
switchSides game with (phase game)
 | (DrawPhase _ _ _) = ?hole {-error case!!-}
 | (MkPhaseCycle _ playerA playerB) = new game (temporaryId playerB) (temporaryId playerA)

{-
{-this is another case where I would like to drill into game having already pattern matched away the possibility that I can't access phase the way I want (pattern matching...)-}
switchSides (MkGame _ _ _ _ _ (MkPhaseCycle _ playerA playerB)) = new game (temporaryId playerB) (temporaryId playerA)
-}

playerOnMove : Game -> WhichPlayer



{-

constructor MkPlayer
 board : Vect 3 (Vect 3 (Maybe Monster))
 rowTarget : Vect 3 (Fin 3)
 hand : List Card
 graveyard : List Card
 discard : List Card
 spawnCard : Maybe Card
 soulCards : Vect 5 Monster
 thoughtsResource : Bounded 0 Preliminaries.absoluteUpperBound
 knowledge : Vect 6 (Bounded 0 9)
 temporaryId : String

-}



{-I'm using tokens to identify players. I should abstract this. It's okay if it's the same data but I should not depend on this being the case.-}
{-This is particularly the case as I'll probably store the player identifier in skills to keep track of the owner. This means that when a player reconnects they need to still be matched even if they have a new token, etc-}

record Battle where
 constructor MkBattle
 round : Round
 game : Game


getOriginalPlayerTemporaryId : WhichPlayer -> Game -> String
getOriginalPlayerTemporaryId whichPlayer game with (phase game)
 | DrawPhase playerA playerB _ =
  case whichPlayer of
   PlayerA => temporaryId playerA
   PlayerB => temporaryId playerB
 | MkPhaseCycle _ playerA playerB =
  case whichPlayer of
   PlayerA => temporaryId playerA
   PlayerB => temporaryId playerB
  


{-
getPlayer : Game -> WhichPlayer -> Player
getPlayer game PlayerA = player_A game
getPlayer game PlayerB = player_B game
-}


{-
updatePlayer : Game -> WhichPlayer -> (Player -> (Player,List ClientUpdate)) -> (Game, List ClientUpdate)
updatePlayer game PlayerA f = let (playerA',updates) = f $ player_A game in
                                  (record {player_A = playerA'} game, updates)
updatePlayer game PlayerB f = let (playerB',updates) = f $ player_B game in
                                  (record {player_B = playerB'} game, updates)





opponent : WhichPlayer -> WhichPlayer
opponent PlayerA = PlayerB
opponent PlayerB = PlayerA

-}

