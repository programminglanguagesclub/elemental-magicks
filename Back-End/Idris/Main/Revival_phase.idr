module Main.Revival_phase
import Data.Fin
import Base.Objects_basic
import Base.Card
import Base.Player
import Main.Game
import Base.Clientupdates
%access public export
%default total


getNumberOfSchools : BasicMonster -> Nat
getNumberOfSchools monster with (schools monster)
 | NoSchools      = 0
 | OneSchool _    = 1
 | TwoSchools _ _ = 2

getReviveCost : (toRevive : List Monster) -> Nat
getReviveCost toRevive = foldl (\n => \m => (n + (getNumberOfSchools (basic m)))) 0 toRevive



{-
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

-}

{-Need to cause units to leave the field if not revived in order of death, and then in order of position on the field. For this we need another data structure in game to represent the order of death-}








stepRevivalPhase : Player -> Player -> (Game, List ClientUpdate)
stepRevivalPhase player opponent = ?hole





