module Draw_phase

import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import phase
import objects_basic
import skill_dsl_data
import player
import game
import serverupdates
import clientupdates
import card
%access public export
%default total


{-



  0     1     1     0     0     1
HA HB HB HA HB HA HA HB HA HB SB SA
  1     0     0     1     1     0
HB HA HA HB HA HB HB HA HB HA SA SB
  0     1     1     0     0     1
HA HB HB HA HB HA HA HB HA HB SB SA
  1     0     0     1     1     0
HB HA HA HB HA HB HB HA HB HA SA SB
  0     1     1     0     0     1
HA HB HB HA HB HA HA HB HA HB SB SA


-}
{-
data CardDraw = AHand | BHand | ASoul | BSoul

swapDrawCommand : CardDraw -> CardDraw
swapDrawCommand AHand = BHand
swapDrawCommand BHand = AHand
swapDrawCommand ASoul = BSoul
swapDrawCommand BSoul = ASoul

maybeSwapDrawCommand : Maybe CardDraw -> Maybe CardDraw
maybeSwapDrawCommand (Just AHand) = Just BHand
maybeSwapDrawCommand (Just BHand) = Just AHand
maybeSwapDrawCommand (Just ASoul) = Just BSoul
maybeSwapDrawCommand (Just BSoul) = Just ASoul
maybeSwapDrawCommand Nothing = Nothing

__getNextTurnDraw : Nat -> Nat -> Maybe CardDraw
__getNextTurnDraw x y = let (a,b) = ((toIntegerNat x),(toIntegerNat y)) in
 if (a == 0 && b == 0) then Just AHand
 else Nothing

_getNextTurnDraw : Game -> Player -> Player -> Maybe CardDraw {-ignoring error case currently-}
_getNextTurnDraw game playerA playerB =
 let (a,b) = (length (hand playerA),length (hand playerB)) in
 let x = __getNextTurnDraw (mod6 a) (mod6 b) in
 let y = modNat (modNat (a+b) 12) 2 in
 if      y == 0
  then x
 else if y == 1
  then maybeSwapDrawCommand x
  else Nothing

getNextTurnDraw : Game -> Maybe CardDraw
getNextTurnDraw game = _getNextTurnDraw game (player_A game) (player_B game)
-}
