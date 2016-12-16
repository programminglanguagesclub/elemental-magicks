module Main.Deployment_phase
import Data.Vect
import Base.Card
import Base.Player
import Main.Game
import Base.Clientupdates
%access public export
%default total

_boardFull : List (Maybe Monster) -> Bool
_boardFull (Nothing::_) = False
_boardFull ((Just m)::tl) = _boardFull tl
_boardFull [] = True

boardFull : Vect 9 (Maybe Monster) -> Bool {-don't want to make players try to deploy if the board is full -}
boardFull board = _boardFull (toList board)


stepDeploymentPhase : Player -> Player -> (Game, List ClientUpdate, Maybe ClientInstruction)
stepDeploymentPhase player opponent = ?hole
