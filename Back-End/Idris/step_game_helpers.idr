module Step_game_helpers

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
%access public export
%default total

damageSoul : (Game, List ClientUpdate) -> Player -> (damage : Nat) -> (Game, List ClientUpdate)

reviveCard : Monster -> Monster

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
{-
data CardDraw = AHand | BHand | ASoul | BSoul

swapDrawCommand : CardDraw -> CardDraw
swapDrawCommand AHand = BHand
swapDrawCommand BHand = AHand
swapDrawCommand ASoul = BSoul
swapDrawCommand BSoul = ASoul

maybeSwapDrawCommand : Maybe CardDraw -> Maybe CardDraw
maybeSwapDrawCommand (Just AHand) = Just BHand
maybeSwapDrawCommand (Just BHand) = Just AHand
maybeSwapDrawCommand (Just ASoul) = Just BSoul
maybeSwapDrawCommand (Just BSoul) = Just ASoul
maybeSwapDrawCommand Nothing = Nothing

__getNextTurnDraw : Nat -> Nat -> Maybe CardDraw
__getNextTurnDraw x y = let (a,b) = ((toIntegerNat x),(toIntegerNat y)) in
 if (a == 0 && b == 0) then Just AHand
 else Nothing

_getNextTurnDraw : Game -> Player -> Player -> Maybe CardDraw {-ignoring error case currently-}
_getNextTurnDraw game playerA playerB =
 let (a,b) = (length (hand playerA),length (hand playerB)) in
 let x = __getNextTurnDraw (mod6 a) (mod6 b) in
 let y = modNat (modNat (a+b) 12) 2 in
 if      y == 0
  then x
 else if y == 1
  then maybeSwapDrawCommand x
  else Nothing

getNextTurnDraw : Game -> Maybe CardDraw
getNextTurnDraw game = _getNextTurnDraw game (player_A game) (player_B game)
-}

{-I think some places I used Bounded 0 25 for hand index. Should change that if I did-}

Selection : {b : Nat} -> {h : Nat} -> {g : Nat} -> (game : Game) -> (Vect b (Fin 9), Vect h (Fin 25), Vect g (Fin 25)) -> (Game, List ClientUpdate)
Selection game (board, hand, graveyard) with (skillHead game)
  | _ = ?hole
 {-| Nothing = ?hole {-(game, [])-}{-probably match on whether it's terminated.-}
 | skill = ?hole
 -}

foo7312016 : Maybe Monster -> Integer
foo7312016 Nothing = 0
foo7312016 (Just m) with (soulPoints ( basic(m) ))
  | (MkBounded (current ** _),_) = current

getPointsFromSoul : Soul -> Integer
getPointsFromSoul n = foldrImpl (\x,y => foo7312016(x)+y) 0 (\x => x) n


getSoulPoints : Player -> Integer
getSoulPoints player = getPointsFromSoul(soul player)

{-
FooDrawCard : Player n m -> Card -> Player (S n) m
FooDrawCard player card = MkPlayer (board player) (reverse (card :: (reverse (hand player)))) (graveyard player) (spawn player) (soul player) (thoughts player) (knowledge player) (temporaryId player)
-}

killFatallyDamaged : (Game, List ClientUpdate) -> (Game, List ClientUpdate)

executeSkillEffects : Game -> List SkillEffect -> (Game, List ClientUpdate)
executeSkillEffects g a = ?hole

skillSelectionPossible : Game -> Condition -> Bool
skillSelectionPossible game condition = ?hole

removeSpawnFromGame : (Game, List ClientUpdate) -> WhichPlayer -> (Game, List ClientUpdate)
removeSpawnFromGame (game, acc) PlayerA  with (spawn (player_A game))
 | Nothing = (game, acc ++ [GameLogicError])
 | Just card = (record {player_A -> discard = (discard (player_A game)) ++ [card], player_A -> spawn = Nothing} game, acc ++ [SendSpawnToDiscard (temporaryId (player_A game))])
removeSpawnFromGame (game, acc) PlayerB with (spawn (player_B game))
 | Nothing = (game, acc ++ [GameLogicError])
 | Just card = (record {player_B -> discard = (discard (player_B game)) ++ [card], player_B -> spawn = Nothing} game, acc ++ [SendSpawnToDiscard (temporaryId (player_B game))])

loadSkill : Game -> (Automatic, Bool, Nat) -> (Game, List ClientUpdate)
loadSkill game = ?hole

_boardFull : List (Maybe Monster) -> Bool
_boardFull (Nothing::_) = False
_boardFull ((Just m)::tl) = _boardFull tl
_boardFull [] = True

boardFull : Vect 9 (Maybe Monster) -> Bool {-don't want to make players try to deploy if the board is full -}
boardFull board = _boardFull (toList board)

_allUnitsDead : List (Maybe Monster) -> Bool
_allUnitsDead (Nothing::tl) = _allUnitsDead tl
_allUnitsDead ((Just m)::tl) with (aliveness (basic m))
 | Alive = False
 | DeadFresh = _allUnitsDead tl
 | DeadStale = _allUnitsDead tl
_allUnitsDead [] = True

allUnitsDead : Vect n (Maybe Monster) -> Bool
allUnitsDead board = _allUnitsDead (toList board)





resetAllSkills : Game -> Game {-Resets start skills, end skills, and counter skills. also decrements engagement (autoskills are reset whenever this happens) -}
possiblyDecrementSoul : Game -> (Game, List ClientUpdate) {-oops, this might have to call step game....-}

{-Might actually want to be stepping round here, not game.-}

goToNextPhase : (Game,List ClientUpdate) -> (Game,List ClientUpdate)
goToNextPhase (game,acc) =
 let (retPhase, phaseUpdate) = nextPhase (phase game) in
 let (game', acc') = (record {phase = retPhase} game, acc ++ [phaseUpdate]) in
 case retPhase of
  SpawnPhase => (record {player_A->thoughts = transformBounded (\x => x + 2) (thoughts (player_A game')),
                         player_B->thoughts = transformBounded (\x => x + 2) (thoughts (player_B game')),
                         turnNumber = S (turnNumber game)}
                        game', acc' ++ [phaseUpdate])
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

getTemporaryIdentifiers : Game -> WhichPlayer -> (String,String)


transformPlayer : (Game,List ClientUpdate) -> WhichPlayer -> (Player -> (Player, List ClientUpdate)) -> (Game, List ClientUpdate)
transformPlayer (game,updateAcc) PlayerA transform =
 let (player,updates) = transform (player_A game) in
 let game' = record {player_A = player} game in
 let updateAcc' = updateAcc ++ updates in
 (game',updateAcc')

transformPlayer (game,updateAcc) PlayerB transform =
 let (player,updates) = transform (player_B game) in
 let game' = record {player_B = player} game in
 let updateAcc' = updateAcc ++ updates in
 (game', updateAcc')




{-This needs to be fixed, but probably should just get numeric typeclasses working first... -}

spendThoughts : (Game,List ClientUpdate) -> WhichPlayer -> Nat -> (Game,List ClientUpdate)
spendThoughts (game, clientUpdates) whichPlayer n =
 transformPlayer (game, clientUpdates)
                 whichPlayer
                 (\p => ((record{thoughts = (thoughts p) {- - n -}} p),
                        clientUpdates ++ [UpdateThoughts (transformBounded (\t => t {- - n -}) (thoughts p)) (temporaryId p)]))





handleSkillInitiation : Game -> Nat -> (Game, List ClientUpdate)

handleSkillSelection : Game -> (List Nat, List Nat, List Nat, List Nat, List Nat, List Nat) -> (Game, List ClientUpdate)

getMonsterField : Player -> Player -> Nat -> Maybe (Player,(Fin 9),Monster)
