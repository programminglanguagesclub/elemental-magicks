module Deployment_phase

import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import phase
import objects_basic
import skill_dsl
import skill_dsl_data
import player
import game
import serverupdates
import clientupdates
import card
%access public export
%default total


_boardFull : List (Maybe Monster) -> Bool
_boardFull (Nothing::_) = False
_boardFull ((Just m)::tl) = _boardFull tl
_boardFull [] = True

boardFull : Vect 9 (Maybe Monster) -> Bool {-don't want to make players try to deploy if the board is full -}
boardFull board = _boardFull (toList board)
