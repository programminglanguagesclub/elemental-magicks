module Main.Game
import Data.Fin
import Data.Vect
import Base.BoundedList
import Base.Skill_dsl_data
import Base.Player
import Base.Phase
import Base.Card
import Base.Clientupdates

%access public export
%default total

-------------------------------------------------------------------------------
data WhichPlayer
 = PlayerA
 | PlayerB
-------------------------------------------------------------------------------
getOpponent : WhichPlayer -> WhichPlayer
getOpponent PlayerA = PlayerB
getOpponent PlayerB = PlayerA
-------------------------------------------------------------------------------
data Round
 = FirstRound
 | SecondRoundOriginalPlayerAWonFirstRound
 | SecondRoundOriginalPlayerBWonFirstRound
-------------------------------------------------------------------------------
{-Reset used_death_skill, used_counter_skill before auto skill and action of card. -}

-------------------------------------------------------------------------------
record Game where
 constructor MkGame
 initiative : WhichPlayer
 turnNumber : Nat
 skillHead : Nonautomatic
 skillQueue : List Automatic
 deathQueue : List Nat {-The temporary ids of the monster (maybe this should have its own type?)-}
 phase : Phase
-------------------------------------------------------------------------------
initialInitiative : WhichPlayer
initialInitiative = PlayerA
-------------------------------------------------------------------------------
initialTurnNumber : Nat
initialTurnNumber = Z
-------------------------------------------------------------------------------
initialSkillHead : Nonautomatic
initialSkillHead = TerminatedSkill
-------------------------------------------------------------------------------
initialSkillQueue : List Automatic
initialSkillQueue = []
-------------------------------------------------------------------------------
initialDeathQueue : List Nat
initialDeathQueue = []
-------------------------------------------------------------------------------
initialPhase :
 String ->
 String ->
 Phase

initialPhase playerAId playerBId =
 let playerA = newDrawPlayer playerAId in
 let playerB = newDrawPlayer playerBId in
 DrawPhase playerA playerB 0
-------------------------------------------------------------------------------
newGame :
 String ->
 String ->
 Game

newGame playerAId playerBId =
 MkGame
  initialInitiative
  initialTurnNumber
  initialSkillHead
  initialSkillQueue
  initialDeathQueue
  (initialPhase playerAId playerBId) 
-------------------------------------------------------------------------------
playerOnMove : Game -> WhichPlayer

playerOnMove = ?hole
-------------------------------------------------------------------------------
record Battle where
 constructor MkBattle
 round : Round
 game : Game
-------------------------------------------------------------------------------
getPlayerTemporaryId :
 WhichPlayer ->
 Game ->
 String

getPlayerTemporaryId whichPlayer game with (phase game)
 | DrawPhase playerA playerB _ =
  case whichPlayer of
   PlayerA => temporaryId playerA
   PlayerB => temporaryId playerB
 | MkPhaseCycle _ playerA playerB =
  case whichPlayer of
   PlayerA => temporaryId playerA
   PlayerB => temporaryId playerB
-------------------------------------------------------------------------------
updatePlayer :
 PhaseCycle ->
 Player ->
 Player ->
 WhichPlayer ->
 (Player -> (Player, List ClientUpdate)) ->
 (Phase, List ClientUpdate)

updatePlayer phase playerA playerB PlayerA mutator =
 let (playerA', clientUpdates) = mutator playerA in
 (MkPhaseCycle phase playerA' playerB, clientUpdates)
updatePlayer phase playerA playerB PlayerB mutator =
 let (playerB', clientUpdates) = mutator playerB in
 (MkPhaseCycle phase playerA playerB', clientUpdates)
-------------------------------------------------------------------------------





