module Transform_game_helpers


import Data.Fin
import Data.Vect
import Data.So
import preliminaries
import phase
import objects_basic
import skill_dsl
import objects_advanced
import serverupdates
import clientupdates
import step_game
import step_game_helpers








public export
schoolsHighEnoughToPlayCard : Player -> Card -> Bool
schoolsHighEnoughToPlayCard player (SpellCard card) = extractBounded (index (school (basic card)) (knowledge player)) >= (extractBounded (level (basic card)))
schoolsHighEnoughToPlayCard player (MonsterCard card) with (schools (basic card))
 | NoSchools = True
 | OneSchool s = extractBounded (index s (knowledge player)) >= (extractBounded (getBaseLevel (level (basic card))))
 | TwoSchools s1 s2 = extractBounded (index s1 (knowledge player)) >= (extractBounded (getBaseLevel (level (basic card)))) && extractBounded (index s2 (knowledge player)) >= (extractBounded (getBaseLevel (level (basic card))))

public export
getNumberOfSchools : BasicMonster -> Nat
getNumberOfSchools monster with (schools monster)
 | NoSchools      = 0
 | OneSchool _    = 1
 | TwoSchools _ _ = 2




{-(positions : List Nat) -> (board : List (Maybe Monster)) -> (acc : Nat) -> Nat-}


public export
getReviveCost : (toRevive : List Monster) -> Nat
getReviveCost toRevive = foldl (\n => \m => (n + (getNumberOfSchools (basic m)))) 0 toRevive

public export
_revive : (positions : List Nat) -> (board : List Card) -> (thoughts : Nat) -> (hand : List Card) -> (graveyard : List Card) -> (acc : Nat) -> (List Card,Nat,List Card,List Card)

public export
revive : (positions : List Nat) -> (board : List Card) -> (thoughts : Nat) -> (hand : List Card) -> (graveyard : List Card) -> (List Card,Nat,List Card,List Card)
revive positions board thoughts hand graveyard = _revive positions board thoughts hand graveyard Z







public export {-this has been deprecated in favor of updatePlayer in step_game_helpers-}
updateGame : Game -> Player -> Game {-figures out which player to modify and does the update-}

{-Need to cause units to leave the field if not revived in order of death, and then in order of position on the field. For this we need another data structure in game to represent the order of death-}

{-For now, completely ignore the possibility of the user using skills! :D -}
public export
_engagementOnMove : (playerABoard : Board) -> (playerBBoard : Board) -> (initiative : WhichPlayer) -> (WhichPlayer, Fin 9)
public export
engagementOnMove : (game : Game) -> (player : Player) -> (opponent : Player) -> (Bool, Fin 9) {-could return a maybe nat, where nothing indicates an error, but I'll trust the ability to not have it the engagement phase if there's nothing next to move-}
public export
_getEnemyStuffInFront : (defenderBoard) -> (row : Fin 3) -> Nat
_getFriendlyStuffInFront : (attackerBoard : Board) -> (attackerSquare : Fin 9) -> Nat 
public export
inRangeRow : (attackerBoard : Board) -> (defenderBoard : Board) -> (attackerSquare : Fin 9) -> (row : Fin 3) -> Maybe Bool
inRangeRow attackerBoard defenderBoard attackerSquare row with (index attackerSquare attackerBoard)
 | Nothing = Nothing
 | Just monster with (aliveness (basic monster))
  | DeadFresh = Nothing
  | DeadStale = Nothing
  | Alive with (range (basic monster))
   | (temporaryRange,_,_) = if gt (fromIntegerNat (extractBounded temporaryRange)) ((_getFriendlyStuffInFront attackerBoard attackerSquare) + (_getEnemyStuffInFront defenderBoard row)) then Just True else Just False


{-
playerOnMove : Game -> Player -> Bool {-assumes engagement phase.. could encode that at type level I suppose-}
-}

public export
moveUnit : (moveFrom : Fin 9) -> (moveTo : Fin 9) -> (board : Board) -> Board {-this actually does a swap-}
moveUnit moveFrom moveTo board = let to = index moveTo board in replaceAt moveFrom to (replaceAt moveTo (index moveFrom board) board)

public export
restUnit : (location : Fin 9) -> Game -> Player {-WhichPlayer-} -> (Game,List ClientUpdate)

public export
_getHandCards : (hand : List Card) -> (acc : MultiTree Nat) -> MultiTree Nat
_getHandCards [] acc = acc
_getHandCards (card::cards) acc with (card)
 |MonsterCard m      = _getHandCards cards (insert acc (permanentId (basic m)))
 |SpellCard s        = _getHandCards cards acc
public export
getHandCards : (hand : List Card) -> MultiTree Nat
getHandCards hand = _getHandCards hand Leaf
public export
__canRevive : (thoughtCost : Nat) -> (thoughts : Thoughts) -> (boardCards : MultiTree Nat) -> (handCards : MultiTree Nat) -> Bool
__canRevive thoughtCost thoughts boardCards handCards = ((extractBoundedNat thoughts) >= thoughtCost) && (dominates handCards boardCards)
public export
_canRevive : Player -> List Bool -> (currentIndex : Nat) -> (thoughtAcc : Nat) -> (boardCardsAcc : MultiTree Nat) -> Bool
_canRevive player [] currentIndex thoughtAcc boardCardsAcc = __canRevive thoughtAcc (thoughts player) boardCardsAcc (getHandCards (hand player))
_canRevive player (x::xs) currentIndex thoughtAcc boardCardsAcc with (x)
 |False             = _canRevive player xs (currentIndex + 1) thoughtAcc boardCardsAcc
 |True with (index' currentIndex (toList (board player)))
  |Nothing          = False {-error-} {-there could be one of two errors: index out of bounds or no monster at location-}
  |Just Nothing     = False
  |Just (Just m) with (aliveness (basic m))
   | Alive          = False
   | DeadFresh      = False {-error-}
   | DeadStale      = _canRevive player xs (currentIndex + 1) (thoughtAcc + (getNumberOfSchools (basic m))) (insert boardCardsAcc (permanentId (basic m)))
public export
canRevive : Player -> Vect 9 Bool -> Bool {-might want to return the cards from the hand as well and the thoughts-}
canRevive player selection = _canRevive player (toList selection) 0 0 Leaf


{-
Rewrite the above code using list comprehension (or at least filter) + fold.

-}

