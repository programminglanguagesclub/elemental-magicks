module Main.Game
import Data.Fin
import Data.Vect
import Base.Bounded
import Base.Hp
import Base.Preliminaries
import Base.BoundedList
import Base.Objects_basic
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
 playerA : Player
 playerB : Player
-------------------------------------------------------------------------------
record DrawPhase where
 constructor MkDrawPhase
 playerA : DrawPlayer
 playerB : DrawPlayer
 turnNumber : Fin 60
-------------------------------------------------------------------------------
data FullGame
 = MkFullGameGame Game
 | MkFullGameDrawPhase DrawPhase
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
initialPhase : Phase
initialPhase = SpawnPhase
-------------------------------------------------------------------------------
newGame :
 String ->
 Vect 5 Monster ->
 Vect 30 Card ->
 String ->
 Vect 5 Monster ->
 Vect 30 Card ->
 Game

newGame aId aHand aSoul bId bHand bSoul =
 MkGame
  initialInitiative
  initialTurnNumber
  initialSkillHead
  initialSkillQueue
  initialDeathQueue
  initialPhase
  (newPlayer aId aHand aSoul)
  (newPlayer bId bHand bSoul)
-------------------------------------------------------------------------------
initialCardsDrawn : Fin 60
initialCardsDrawn = 0
-------------------------------------------------------------------------------
newDrawPhase : String -> String -> DrawPhase
newDrawPhase aId bId = MkDrawPhase (newDrawPlayer aId) (newDrawPlayer bId) initialCardsDrawn
-------------------------------------------------------------------------------
playerOnMove : Game -> WhichPlayer

playerOnMove = ?hole
-------------------------------------------------------------------------------
record Battle where
 constructor MkBattle
 round : Round
 game : FullGame
-------------------------------------------------------------------------------
getPlayer :
 WhichPlayer ->
 Game ->
 Player

getPlayer PlayerA game = playerA game
getPlayer PlayerB game = playerB game
-------------------------------------------------------------------------------
getPlayerTemporaryId :
 WhichPlayer ->
 Game ->
 String

getPlayerTemporaryId whichPlayer gameCycle =
 temporaryId $ getPlayer whichPlayer gameCycle
-------------------------------------------------------------------------------
updatePlayer :
 WhichPlayer ->
 Game ->
 (Player -> (Player, List ClientUpdate)) ->
 (Game, List ClientUpdate)
{-
updatePlayer phase playerA playerB PlayerA mutator =
 let (playerA', clientUpdates) = mutator playerA in
 (MkPhaseCycle phase playerA' playerB, clientUpdates)
updatePlayer phase playerA playerB PlayerB mutator =
 let (playerB', clientUpdates) = mutator playerB in
 (MkPhaseCycle phase playerA playerB', clientUpdates)
-}
-------------------------------------------------------------------------------
-- Move these helper functions to card. These also get used in the DSL.

modifyHp :
 (Integer -> Integer) ->
 Monster ->
 Monster

modifyHp mutator monster =
 record {basic -> hp $= transformHp mutator} monster
-------------------------------------------------------------------------------
subtractHp :
 Nat ->
 Monster ->
 (Bool, Monster)

subtractHp value monster with (value)
 | Z = (False, monster)
 | S _ = (True, modifyHp (\x => x - (toIntegerNat value)) monster)
-------------------------------------------------------------------------------
fatallyDamaged : Monster -> Bool
fatallyDamaged monster = (getCurrentHp $ hp $ basic monster) <= 0
-------------------------------------------------------------------------------
damageCard :
 Nat ->
 Fin 3 ->
 Fin 3 ->
 Game ->
 WhichPlayer ->
 (List ClientUpdate, Game)

{-
damageCard damage row column game whichPlayer with (indexMonster row column player)
 | Nothing = ([], player)
 | Just monster =
  let defenderDefense = getTemporary $ defense $ basic monster in
  let hpLost = removeUpperBound ((toIntegerNat damage) - defenderDefense) in -- this will not allow for extra damage outside the bound of defense.... wrong code.
  let (fatallyDamaged, damagedMonster) = subtractHp hpLost monster in
  let damagedCardUpdate = the ClientUpdate ?hole in
  case fatallyDamaged of
   True =>
    case getCanUseDeathSkill damagedMonster of
     Nothing => ([damagedCardUpdate], ?hole)
     Just skill =>
      let damagedMonster' = setCanUseDeathSkill damagedMonster False in
      ?hole
   False =>
    case getCanUseCounterSkill damagedMonster of
     Nothing => ([damagedCardUpdate], ?hole)
     Just skill =>
      let damagedMonster' = setCanUseCounterSkill damagedMonster False in
      ?hole
-} 
  {- if hp > 0, and has counter skill, then see if counter skill has been used. otherwise same with death skill. -}


-- this function probably needs to be able to modify more things..

-------------------------------------------------------------------------------
applyAttack :
 Bounded 0 Preliminaries.absoluteUpperBound ->
 Fin 3 ->
 Player ->
 (List ClientUpdate, Player)

applyAttack atk row defendingPlayer = ?hole

-- this function also probably needs to be able to modify more things.

-------------------------------------------------------------------------------



