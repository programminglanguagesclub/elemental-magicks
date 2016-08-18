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
 skillHead  : Maybe (Condition, SkillComponent, SkillComponent, SkillComponent)
 skillQueue : List SkillComponent
 deathQueue : List Nat {-The temporary ids of the monster (maybe this should have its own type?)-}
 player_A   : Player
 player_B   : Player
 phase      : Phase
 
syntax "new" "game" [tokenA] [tokenB] = MkGame PlayerA 0 (Vect.Nil,Vect.Nil,Vect.Nil) Nothing [] [] (new player tokenA) (new player tokenB) DrawPhase

{-MIGHT WANT THE PLAYERS IN A TUPLE OF PLAYERS-}

playerOnMove : Game -> WhichPlayer
























public export
evaluateLazyInt : LazyInt -> (player : Player) -> (opponent : Player) -> Maybe Integer
evaluateLazyInt (LazyIntStat lazyIntStatType env statRValue nat) _ _ with (index' nat env)
 |Nothing = Nothing
 |Just basicMonster = Just (getStat lazyIntStatType statRValue basicMonster)
evaluateLazyInt (Constant integer) _ _ = Just integer
evaluateLazyInt (ThoughtsR CardOwner) player _ = Just (extractBounded (thoughts player))
evaluateLazyInt (ThoughtsR CardOpponent) _ opponent = Just (extractBounded (thoughts opponent))
evaluateLazyInt (SchoolR CardOwner school) player _ = {-Just (extractBounded (index school (knowledge player)))-} let x = index school (knowledge player) in Just 0
{- temporarily disabled .... evaluateLazyInt (SchoolR CardOpponent school) _ opponent = Just (extractBounded (index school (knowledge opponent))) -}


{- {-from skill_dsl:-}

public export
data SkillEffect = AttackL  Env Mutator StatLValue BoardMonsterVar LazyInt
                 | DefenseL Env Mutator StatLValue BoardMonsterVar LazyInt
                 | RangeL   Env Mutator StatLValue BoardMonsterVar LazyInt
                 | LevelL   Env Mutator StatLValue BoardMonsterVar LazyInt
                 | SpeedL   Env Mutator StatLValue BoardMonsterVar LazyInt

public export
data SkillComponent = SkillComponent_ (List SkillEffect, Maybe (Condition, SkillComponent, SkillComponent, SkillComponent)) String {-the string here is the temporaryId of the player in the game-}


{-executeSkillEffectTransform : SkillEffect -> BasicMonster -> Maybe BasicMonster {-Nothing indicates a logic error-} {-This function already exists 
-}

in objects_advanced, but handles integration with the game, etc.-} {-Probably this needs to go-}
{-executeSkillEffectTransform (AttackL env mutator statLValue boardMonsterVar lazyInt) basicMonster with (mutator, statLValue, )-}






executeSkillEffectTransform (SkillEffectStatL AttackL env mutator statLValue (UnBoundBoardMonsterVar _ _) lazyInt) _ = Nothing

-}


public export
getIndexOfMonster' : Nat -> Board -> Maybe (Fin 9)
getIndexOfMonster' nat board = head' (findIndices (\m => True) board)

public export
getIndexOfMonster : Player -> BoardMonsterVar -> Maybe (Fin 9)
getIndexOfMonster _ (UnBoundBoardMonsterVar _ _) = Nothing
getIndexOfMonster player (BoundBoardMonsterVar nat) = getIndexOfMonster' nat (board player)




public export
executeSkillEffect : Game -> (player : Player) -> (opponent : Player) -> SkillEffect -> (Game, List ClientUpdate)
executeSkillEffect game player opponent (SkillEffectStatL skillEffectStatType env mutator statLValue boardMonsterVar lazyInt) with (getIndexOfMonster player boardMonsterVar)
 | Nothing = (game, [GameLogicError])
 | Just indexOfMonster with (skillEffectStatType)
  | AttackL  = ?g
  | DefenseL = ?g
  | RangeL   = ?g
  | LevelL   = ?g
  | SpeedL   = ?g



{-I might already have a hole by this name-}



{-
public export
executeSkillEffect : Game -> SkillEffect -> (Game, List ClientUpdate)
executeSkillEffect game skillEffect with (skillEffect)
 |AttackL env mutator statLValue boardMonsterVar lazyInt = (game,[])
 |_ = (game,[])

-}



{-One of the things I need to do here is have functionality for getting the owner player and opponent player-}


{-This should use executeSkillEffectTransform to replace the monster affected in the game-}













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



