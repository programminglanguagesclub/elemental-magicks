module Main.Deployment_phase
import Data.Vect
import Base.Card
%access public export
%default total

_boardFull : List (Maybe Monster) -> Bool
_boardFull (Nothing::_) = False
_boardFull ((Just m)::tl) = _boardFull tl
_boardFull [] = True

boardFull : Vect 9 (Maybe Monster) -> Bool {-don't want to make players try to deploy if the board is full -}
boardFull board = _boardFull (toList board)
