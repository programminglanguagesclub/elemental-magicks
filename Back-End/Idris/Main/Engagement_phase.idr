module Main.Engagement_phase

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




_allUnitsDead : List (Maybe Monster) -> Bool
_allUnitsDead (Nothing::tl) = _allUnitsDead tl
_allUnitsDead ((Just m)::tl) with (aliveness (basic m))
 | Alive = False
 | DeadFresh = _allUnitsDead tl
 | DeadStale = _allUnitsDead tl
_allUnitsDead [] = True

allUnitsDead : Vect n (Maybe Monster) -> Bool
allUnitsDead board = _allUnitsDead (toList board)
