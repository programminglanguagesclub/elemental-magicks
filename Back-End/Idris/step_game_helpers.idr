module Step_game_helpers

import Data.Vect
import preliminaries
import phase
import objects_basic
import skill_dsl
import objects_advanced
import serverupdates
import clientupdates




public export
damageSoul : (Game, List ClientUpdate) -> Player -> (damage : Nat) -> (Game, List ClientUpdate)


public export
reviveCard : Monster -> Monster



public export
updatePlayer : Game -> Player -> (Player -> Player) -> Game {-applies update to player_A or player_B, as appropriate-}

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
public export
data CardDraw = AHand | BHand | ASoul | BSoul
public export
swapDrawCommand : CardDraw -> CardDraw
swapDrawCommand AHand = BHand
swapDrawCommand BHand = AHand
swapDrawCommand ASoul = BSoul
swapDrawCommand BSoul = ASoul
public export
maybeSwapDrawCommand : Maybe CardDraw -> Maybe CardDraw
maybeSwapDrawCommand (Just AHand) = Just BHand
maybeSwapDrawCommand (Just BHand) = Just AHand
maybeSwapDrawCommand (Just ASoul) = Just BSoul
maybeSwapDrawCommand (Just BSoul) = Just ASoul
maybeSwapDrawCommand Nothing = Nothing
public export
__getNextTurnDraw : Nat -> Nat -> Maybe CardDraw
__getNextTurnDraw x y = let (a,b) = ((toIntegerNat x),(toIntegerNat y)) in
 if (a == 0 && b == 0) then Just AHand
 else Nothing
public export
_getNextTurnDraw : Game -> Player -> Player -> Maybe CardDraw {-ignoring error case currently-}
_getNextTurnDraw game playerA playerB =
 let (a,b) = (length (hand playerA),length (hand playerB)) in
 let x = __getNextTurnDraw (modNat a 6) (modNat b 6) in
 let y = modNat (modNat (a+b) 12) 2 in
 if      y == 0
  then x
 else if y == 1
  then maybeSwapDrawCommand x
  else Nothing
public export
getNextTurnDraw : Game -> Maybe CardDraw
getNextTurnDraw game = _getNextTurnDraw game (player_A game) (player_B game)
public export
Selection : {b : Nat} -> {h : Nat} -> {g : Nat} -> (game : Game) -> (Vect b BoardIndex, Vect h HandIndex, Vect g GraveyardIndex) -> (Game, List ClientUpdate)
Selection game (board, hand, graveyard) with (skillHead game)
 | Nothing = ?hole {-(game, [])-}
 | skill = ?hole
public export
getSoulPoints : Player -> Nat
getSoulPoints player = ?hole

{-
FooDrawCard : Player n m -> Card -> Player (S n) m
FooDrawCard player card = MkPlayer (board player) (reverse (card :: (reverse (hand player)))) (graveyard player) (spawn player) (soul player) (thoughts player) (knowledge player) (temporaryId player)
-}
public export
killFatallyDamaged : (Game, List ClientUpdate) -> (Game, List ClientUpdate)
public export
executeSkillEffects : Game -> List SkillEffect -> (Game, List ClientUpdate)
executeSkillEffects g a = ?hole
public export
skillSelectionPossible : Game -> Condition -> Bool
skillSelectionPossible game condition = ?hole
public export
removeSpawnFromGame : (Game, List ClientUpdate) -> WhichPlayer -> (Game, List ClientUpdate)
removeSpawnFromGame (game, acc) PlayerA  with (spawn (player_A game))
 | Nothing = (game, acc ++ [GameLogicError])
 | Just card = (record {player_A -> discard = (discard (player_A game)) ++ [card], player_A -> spawn = Nothing} game, acc ++ [SendSpawnToDiscard (temporaryId (player_A game)) (temporaryId (player_B game))])
removeSpawnFromGame (game, acc) PlayerB with (spawn (player_B game))
 | Nothing = (game, acc ++ [GameLogicError])
 | Just card = (record {player_B -> discard = (discard (player_B game)) ++ [card], player_B -> spawn = Nothing} game, acc ++ [SendSpawnToDiscard (temporaryId (player_B game)) (temporaryId (player_A game))])
public export
loadSkill : Game -> (SkillComponent, Bool, Nat) -> (Game, List ClientUpdate)
loadSkill game = ?hole
public export
_boardFull : List (Maybe Monster) -> Bool
_boardFull (Nothing::_) = False
_boardFull ((Just m)::tl) = _boardFull tl
_boardFull [] = True
public export
boardFull : Board -> Bool {-don't want to make players try to deploy if the board is full -}
boardFull board = _boardFull (toList board)
public export
_allUnitsDead : List (Maybe Monster) -> Bool
_allUnitsDead (Nothing::tl) = _allUnitsDead tl
_allUnitsDead ((Just m)::tl) with (aliveness (basic m))
 | Alive = False
 | DeadFresh = _allUnitsDead tl
 | DeadStale = _allUnitsDead tl
_allUnitsDead [] = True
public export
allUnitsDead : Vect n (Maybe Monster) -> Bool
allUnitsDead board = _allUnitsDead (toList board)





resetAllSkills : Game -> Game {-Resets start skills, end skills, and counter skills. also decrements engagement (autoskills are reset whenever this happens) -}
possiblyDecrementSoul : Game -> (Game, List ClientUpdate) {-oops, this might have to call step game....-}

{-Might actually want to be stepping round here, not game.-}
public export
goToNextPhase : (Game,List ClientUpdate) -> (Game,List ClientUpdate)
goToNextPhase (game,acc) =
 let (retPhase, phaseUpdate) = nextPhase (phase game) in
 let (game', acc') = (record {phase = retPhase} game, acc ++ [phaseUpdate]) in
 case retPhase of
  SpawnPhase => (record {player_A->thoughts = transformThoughts (\x => x + 2) (thoughts (player_A game')),player_B->thoughts = transformThoughts (\x => x + 2) (thoughts (player_B game')), turnNumber = S (turnNumber game)} game', acc' ++ [phaseUpdate])
  SpellPhase => (game', acc')
  RemovalPhase => (game', acc')
  StartPhase => (resetAllSkills game', acc')
  EngagementPhase => (game', acc')
  EndPhase => (game', acc')
  RevivalPhase => (game', acc')
  DeploymentPhase => (game', acc')


{-



HAVE TO SET DEATHSTALE/FRESH SOMEWHERE WHEN TRANSITIONING PHASES
Also have to possibly decrement soul points depending on what turn it is.


-}

{-sendSpawnToGraveyard : (Game, List ClientUpdate) -> WhichPlayer -> (Game, List ClientUpdate)-}

public export
getTemporaryIdentifiers : Game -> WhichPlayer -> (String,String)

public export
spendThoughts : (Game,List ClientUpdate) -> WhichPlayer -> Nat -> (Game,List ClientUpdate)

public export
handleSkillInitiation : Game -> Nat -> (Game, List ClientUpdate)


public export
handleSkillSelection : Game -> (List Nat, List Nat, List Nat, List Nat, List Nat, List Nat) -> (Game, List ClientUpdate)
public export
getMonsterField : Player -> Player -> Nat -> Maybe (Player,(Fin 9),Monster)
