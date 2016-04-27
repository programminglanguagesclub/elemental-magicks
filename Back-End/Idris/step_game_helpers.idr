module Step_game_helpers

import Data.Vect
import preliminaries
import phase
import objects_basic
import skill_dsl
import objects_advanced
import serverupdates
import clientupdates



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
FooDrawCard player card = MkPlayer (board player) (reverse (card :: (reverse (hand player)))) (graveyard player) (spawn player) (soul player) (thoughts player) (knowledge player) (token player)
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
 | Just card = (record {player_A -> discard = (discard (player_A game)) ++ [card], player_A -> spawn = Nothing} game, acc ++ [SendSpawnToDiscard (token (player_A game)) (token (player_B game))])
removeSpawnFromGame (game, acc) PlayerB with (spawn (player_B game))
 | Nothing = (game, acc ++ [GameLogicError])
 | Just card = (record {player_B -> discard = (discard (player_B game)) ++ [card], player_B -> spawn = Nothing} game, acc ++ [SendSpawnToDiscard (token (player_B game)) (token (player_A game))])
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

{-Might actually want to be stepping round here, not game.-}
public export
goToNextPhase : (Game,List ClientUpdate) -> (Game,List ClientUpdate)
goToNextPhase (game,acc) =
 let (retPhase, phaseUpdate) = nextPhase (phase game) in
 let (game', acc') = (record {phase = retPhase} game, acc ++ [phaseUpdate]) in
 case retPhase of
  SpawnPhase => (record {player_A->thoughts = transformThoughts (\x => x + 2) (thoughts (player_A game')),player_B->thoughts = transformThoughts (\x => x + 2) (thoughts (player_B game'))} game', acc' ++ [phaseUpdate])

{-HAVE TO GIVE EXTRA THOUGHTS AND SET DEATHSTALE/FRESH SOMEWHERE WHEN TRANSITIONING PHASES
Also have to reset counter skills (so they can trigger again) probably reset auto skills as well? Maybe that can happen when cards are disengaged (so that soul skills can also cause that to happen)
Also have to change the turn count, and possibly decrement soul points depending on what turn it is.
-}

{-sendSpawnToGraveyard : (Game, List ClientUpdate) -> WhichPlayer -> (Game, List ClientUpdate)-}

public export
getTokens : Game -> WhichPlayer -> (String,String)

public export
spendThoughts : (Game,List ClientUpdate) -> WhichPlayer -> Nat -> (Game,List ClientUpdate)

public export
handleSkillInitiation : Game -> Nat -> (Game, List ClientUpdate)


public export
handleSkillSelection : Game -> (List Nat, List Nat, List Nat, List Nat, List Nat, List Nat) -> (Game, List ClientUpdate)
public export
damageSoul : Game -> Player -> (damage : Nat) -> (Game, List ClientUpdate)
public export
getMonsterField : Player -> Player -> Nat -> Maybe (Player,(Fin 9),Monster)
