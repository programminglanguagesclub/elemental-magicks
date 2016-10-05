module Main.Transform_game_helpers

import Data.Fin
import Data.Vect
import Data.So
import Base.Bounded
import Base.Bounded_then_integer
import Base.Integer_then_bounded
import Base.Preliminaries
import Base.Phase
import Base.Objects_basic
import Base.Player
import Base.Card
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
import Main.Step_game
import Main.Step_game_helpers
%access public export
%default total

schoolsHighEnoughToPlayCard : Player -> Card -> Bool
schoolsHighEnoughToPlayCard player (SpellCard card) = geq (index (school $ basic card) $ knowledge player) (level $ basic card)
schoolsHighEnoughToPlayCard player (MonsterCard card) with (schools (basic card))
 | NoSchools = True
 | OneSchool s = geq (index s $ knowledge player) (snd $ snd $ level $ basic card)
 | TwoSchools s1 s2 = geq (index s1 $ knowledge player) (snd $ snd $ level $ basic card) && geq (index s2 $ knowledge player) (snd $ snd $ level $ basic card)



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
 |MonsterCard m      = _getHandCards cards (insert acc (id (basic m)))
 |SpellCard s        = _getHandCards cards acc
getHandCards : (hand : List Card) -> MultiTree Nat
getHandCards hand = _getHandCards hand Leaf

