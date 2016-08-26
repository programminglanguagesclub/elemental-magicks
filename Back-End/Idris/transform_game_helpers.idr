module Transform_game_helpers

import Data.Fin
import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import phase
import objects_basic
import skill_dsl
import player
import game
import serverupdates
import clientupdates
import step_game
import step_game_helpers
%access public export
%default total

schoolsHighEnoughToPlayCard : Player -> Card -> Bool
schoolsHighEnoughToPlayCard player (SpellCard card) = geq (index (school $ basic card) $ knowledge player) (level $ basic card)
schoolsHighEnoughToPlayCard player (MonsterCard card) with (schools (basic card))
 | NoSchools = True
 | OneSchool s = geq (index s $ knowledge player) (snd $ snd $ level $ basic card)
 | TwoSchools s1 s2 = geq (index s1 $ knowledge player) (snd $ snd $ level $ basic card) && geq (index s2 $ knowledge player) (snd $ snd $ level $ basic card)

getNumberOfSchools : BasicMonster -> Nat
getNumberOfSchools monster with (schools monster)
 | NoSchools      = 0
 | OneSchool _    = 1
 | TwoSchools _ _ = 2

getReviveCost : (toRevive : List Monster) -> Nat
getReviveCost toRevive = foldl (\n => \m => (n + (getNumberOfSchools (basic m)))) 0 toRevive


{-I might want to move these to the graceyard rather than simply removing them from the hand-}

_removeMonsterFromHandByPermanentId : (acc : List Card) -> (hand : List Card) -> (id : Nat) -> (Maybe Monster, List Card)
_removeMonsterFromHandByPermanentId [] _ _ = (Nothing,[])
_removeMonsterFromHandByPermanentId acc ((SpellCard _)::xs) id = _removeMonsterFromHandByPermanentId acc xs id
_removeMonsterFromHandByPermanentId acc ((MonsterCard m)::xs) id = if (permanentId (basic m)) == id then (Just m,acc ++ xs) else _removeMonsterFromHandByPermanentId (acc ++ [MonsterCard m]) xs id
{-could optimize this to use :: instead of ++ and then reverse at the end-}

removeMonsterFromHandByPermanentId : (hand : List Card) -> (id : Nat) -> (Maybe Monster, List Card)
removeMonsterFromHandByPermanentId = _removeMonsterFromHandByPermanentId []


moveMonsterFromHandToGraveyardByPermanentId : (hand : List Card) -> (graveyard : List Card) -> (id : Nat) -> Maybe (List Card, List Card)
moveMonsterFromHandToGraveyardByPermanentId hand graveyard id =
 case removeMonsterFromHandByPermanentId hand id of
      (Nothing, hand') => Nothing
      (Just m, hand') => Just (hand', (graveyard ++ [MonsterCard m]))


reviveMonster : Monster -> Monster


_revive : Vect 9 Bool -> Vect 9 (Maybe Monster) -> Thoughts -> List Card -> List Card -> Maybe (Vect 9 (Maybe Monster),Thoughts,List Card,List Card)
_revive positions board thoughts hand graveyard =
 let zipped = zipWith reviveSelectedMonsters positions board in
 let revivedMonsters = justRevivedMonsters (toList zipped) in
 (case moveMonsterFromHandToGraveyardByPermanentId hand graveyard ?hole of
      Nothing => Nothing
      Just (hand', graveyard') => Just (board, thoughts, hand, graveyard)) where
  reviveSelectedMonsters : Bool -> Maybe Monster -> Maybe Monster
  reviveSelectedMonsters True (Just m) = Just (reviveMonster m)
  reviveSelectedMonsters _ _ = Nothing
  justRevivedMonsters : List (Maybe Monster) -> List Monster
  justRevivedMonsters [] = []
  justRevivedMonsters (Nothing::xs) = justRevivedMonsters xs
  justRevivedMonsters ((Just m)::xs) = [m] ++ (justRevivedMonsters xs)
  {-removeRevivedCards : List Monster -> List Card -> List Card -> (List Card, List Card)
  removeRevivedCards [] hand graveyard = (hand, graveyard)
  removeRevivedCards (x::xs) hand graveyard = ?h
  -}

revive : (positions : Vect 9 Bool) -> (player : Player) -> Maybe Player
revive positions player = case _revive positions (board player) (thoughts player) (hand player) (graveyard player) of
                               Nothing => Nothing
                               Just (board', thoughts', hand', graveyard') => Just (record {board = board', thoughts = thoughts', hand = hand', graveyard = graveyard'} player)



{-Need to cause units to leave the field if not revived in order of death, and then in order of position on the field. For this we need another data structure in game to represent the order of death-}

{-For now, completely ignore the possibility of the user using skills! :D -}
_engagementOnMove : (playerABoard : Vect 9 (Maybe Monster)) -> (playerBBoard : Vect 9 (Maybe Monster)) -> (initiative : WhichPlayer) -> (WhichPlayer, Fin 9)
engagementOnMove : (game : Game) -> (player : Player) -> (opponent : Player) -> (Bool, Fin 9) {-could return a maybe nat, where nothing indicates an error, but I'll trust the ability to not have it the engagement phase if there's nothing next to move-}
_getEnemyStuffInFront : (defenderBoard) -> (row : Fin 3) -> Nat
_getFriendlyStuffInFront : (attackerBoard : Vect 9 (Maybe Monster)) -> (attackerSquare : Fin 9) -> Nat 
inRangeRow : (attackerBoard : Vect 9 (Maybe Monster)) -> (defenderBoard : Vect 9 (Maybe Monster)) -> (attackerSquare : Fin 9) -> (row : Fin 3) -> Maybe Bool
inRangeRow attackerBoard defenderBoard attackerSquare row with (index attackerSquare attackerBoard)
 | Nothing = Nothing
 | Just monster with (aliveness (basic monster))
  | DeadFresh = Nothing
  | DeadStale = Nothing
  | Alive with (range (basic monster))
   | (temporaryRange,_,_) = if gt (fromIntegerNat (extractBounded temporaryRange)) ((_getFriendlyStuffInFront attackerBoard attackerSquare) + (_getEnemyStuffInFront defenderBoard row)) then Just True else Just False

moveUnit : (moveFrom : Fin 9) -> (moveTo : Fin 9) -> (board : Vect 9 (Maybe Monster)) -> Vect 9 (Maybe Monster) {-this actually does a swap-}
moveUnit moveFrom moveTo board = let to = index moveTo board in replaceAt moveFrom to (replaceAt moveTo (index moveFrom board) board)

restUnit : (location : Fin 9) -> Game -> WhichPlayer -> (Game,List ClientUpdate)

_getHandCards : (hand : List Card) -> (acc : MultiTree Nat) -> MultiTree Nat
_getHandCards [] acc = acc
_getHandCards (card::cards) acc with (card)
 |MonsterCard m      = _getHandCards cards (insert acc (permanentId (basic m)))
 |SpellCard s        = _getHandCards cards acc
getHandCards : (hand : List Card) -> MultiTree Nat
getHandCards hand = _getHandCards hand Leaf

