module Main.Deployment_phase

import Data.Vect
import Data.So
import Base.Bounded
import Base.Bounded_then_integer
import Base.Integer_then_bounded
import Base.Preliminaries
import Base.Phase
import Base.Objects_basic
import Base.Skill_dsl_data
import Base.Player
import Main.Game
import Main.Serverupdates
import Base.Clientupdates
import Base.Card
%access public export
%default total


_boardFull : List (Maybe Monster) -> Bool
_boardFull (Nothing::_) = False
_boardFull ((Just m)::tl) = _boardFull tl
_boardFull [] = True

boardFull : Vect 9 (Maybe Monster) -> Bool {-don't want to make players try to deploy if the board is full -}
boardFull board = _boardFull (toList board)
