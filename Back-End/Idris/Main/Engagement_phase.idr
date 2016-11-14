module Main.Engagement_phase
import Data.Vect
import Base.Objects_basic
import Base.Card
import Base.Player
import Main.Game
import Base.Clientupdates
%access public export
%default total

_allUnitsDead : List (Maybe Monster) -> Bool
_allUnitsDead (Nothing::tl) = _allUnitsDead tl
_allUnitsDead ((Just m)::tl) with (aliveness (basic m))
 | Alive = False
 | DeadFresh = _allUnitsDead tl
 | DeadStale = _allUnitsDead tl
_allUnitsDead [] = True

allUnitsDead : Vect n (Maybe Monster) -> Bool
allUnitsDead board = _allUnitsDead (toList board)



stepEngagementPhase : WhichPlayer -> List Nat -> Player -> Player -> (Game, List ClientUpdate)
stepEngagementPhase initiative deathQueue player opponent = ?hole
