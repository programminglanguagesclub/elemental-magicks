module Game

import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import objects_basic
import skill_dsl_data
import player
import skill_dsl_logic
import skill_dsl
import phase
import clientupdates
import card_list



public export
data WhichPlayer = PlayerA | PlayerB
public export
data Round = FirstRound | SecondRound


{-Reset used_death_skill, used_counter_skill before auto skill and action of card. -}


public export
record Game where
 constructor MkGame
 initiative : WhichPlayer
 turnNumber : Nat
 skillHead  : Nonautomatic 
 skillQueue : List Automatic
 deathQueue : List Nat {-The temporary ids of the monster (maybe this should have its own type?)-}
 player_A   : Player
 player_B   : Player
 phase      : Phase
 
syntax "new" "game" [tokenA] [tokenB] = MkGame PlayerA 0 (Vect.Nil,Vect.Nil,Vect.Nil) Nothing [] [] (new player tokenA) (new player tokenB) DrawPhase

{-MIGHT WANT THE PLAYERS IN A TUPLE OF PLAYERS-}

playerOnMove : Game -> WhichPlayer


{-I'm using tokens to identify players. I should abstract this. It's okay if it's the same data but I should not depend on this being the case.-}
{-This is particularly the case as I'll probably store the player identifier in skills to keep track of the owner. This means that when a player reconnects they need to still be matched even if they have a new token, etc-}

public export
record Battle where
 constructor MkBattle
 round                : Round
 originalPlayerAToken : String
 originalPlayerBToken : String
 game                 : Game

public export
getPlayer : Game -> WhichPlayer -> Player
getPlayer game PlayerA = player_A game
getPlayer game PlayerB = player_B game

public export
opponent : WhichPlayer -> WhichPlayer
opponent PlayerA = PlayerB
opponent PlayerB = PlayerA



