module Main.Game
import Base.Skill_dsl_data
import Base.Player
import Base.Phase
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
 skillHead : Nonautomatic {-
 currentEvoker : Nat {-this would be better in skillHead but that is somewhat invasive when I create skills..-}
Actually I'm going to put this in them..
-}
 skillQueue : List Automatic
 deathQueue : List Nat {-The temporary ids of the monster (maybe this should have its own type?)-}
 phase : Phase
 

{- This is out of date... -}
syntax "new" "game" [playerAId] [playerBId] = MkGame PlayerA 0 TerminatedSkill [] [] (new player playerAId) (new player playerBId) DrawPhase

{-MIGHT WANT THE PLAYERS IN A TUPLE OF PLAYERS-}

playerOnMove : Game -> WhichPlayer


{-I'm using tokens to identify players. I should abstract this. It's okay if it's the same data but I should not depend on this being the case.-}
{-This is particularly the case as I'll probably store the player identifier in skills to keep track of the owner. This means that when a player reconnects they need to still be matched even if they have a new token, etc-}

record Battle where
 constructor MkBattle
 round : Round
 originalPlayerAToken : String
 originalPlayerBToken : String
 game : Game

getPlayer : Game -> WhichPlayer -> Player
getPlayer game PlayerA = player_A game
getPlayer game PlayerB = player_B game


updatePlayer : Game -> WhichPlayer -> (Player -> (Player,List ClientUpdate)) -> (Game, List ClientUpdate)
updatePlayer game PlayerA f = let (playerA',updates) = f $ player_A game in
                                  (record {player_A = playerA'} game, updates)
updatePlayer game PlayerB f = let (playerB',updates) = f $ player_B game in
                                  (record {player_B = playerB'} game, updates)

opponent : WhichPlayer -> WhichPlayer
opponent PlayerA = PlayerB
opponent PlayerB = PlayerA



