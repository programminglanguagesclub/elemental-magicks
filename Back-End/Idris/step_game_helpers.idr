module Step_game_helpers

import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import phase
import objects_basic
import skill_dsl_data
import player
import game
import serverupdates
import clientupdates
import card
import draw_phase
import spawn_phase
import spell_phase
import removal_phase
import start_phase
import engagement_phase
import end_phase
import revival_phase
import deployment_phase
%access public export
%default total

damageSoul : (Game, List ClientUpdate) -> Player -> (damage : Nat) -> (Game, List ClientUpdate)



updatePlayer : Game -> Player -> (Player -> Player) -> Game {-applies update to player_A or player_B, as appropriate-}



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
getSoulPoints player = getPointsFromSoul(soulCards player)

{-
FooDrawCard : Player n m -> Card -> Player (S n) m
FooDrawCard player card = MkPlayer (board player) (reverse (card :: (reverse (hand player)))) (graveyard player) (spawn player) (soul player) (thoughts player) (knowledge player) (temporaryId player)
-}

killFatallyDamaged : (Game, List ClientUpdate) -> (Game, List ClientUpdate)
{-
removeSpawnFromGame : (Game, List ClientUpdate) -> WhichPlayer -> (Game, List ClientUpdate)
removeSpawnFromGame (game, acc) PlayerA with (spawnCard (player_A game))
 | Nothing = (game, acc ++ [GameLogicError])
 | Just card = (record {player_A -> discard = (discard (player_A game)) ++ [card], player_A -> spawn = Nothing} game, acc ++ [SendSpawnToDiscard (temporaryId (player_A game))])
removeSpawnFromGame (game, acc) PlayerB with (spawn (player_B game))
 | Nothing = (game, acc ++ [GameLogicError])
 | Just card = (record {player_B -> discard = (discard (player_B game)) ++ [card], player_B -> spawn = Nothing} game, acc ++ [SendSpawnToDiscard (temporaryId (player_B game))])

loadSkill : Game -> (Automatic, Bool, Nat) -> (Game, List ClientUpdate)
loadSkill game = ?hole
-}







resetAllSkills : Game -> Game {-Resets start skills, end skills, and counter skills. also decrements engagement (autoskills are reset whenever this happens) -}
possiblyDecrementSoul : Game -> (Game, List ClientUpdate) {-oops, this might have to call step game....-}

{-Might actually want to be stepping round here, not game.-}
{-THIS IS WHERE I NEED TO BE LOADING THE SPAWN SKILLS!!!!-}
goToNextPhase : (Game,List ClientUpdate) -> (Game,List ClientUpdate)
goToNextPhase (game,acc) = ?hole
 {- let (retPhase, phaseUpdate) = nextPhase (phase game) in
    let (game', acc') = (record {phase = retPhase} game, acc ++ [phaseUpdate]) in
      case retPhase of
           DrawPhase => (game', acc')

  {-am I sending the ++ turnNumber and += 2 thoughts updates???-}
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
           -}

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



spendThoughts : Player -> Integer -> (Player, List ClientUpdate)
spendThoughts player val = ((record {thoughtsResource = {- (\t => t - val) -} ( >> 0 << ) {-(thoughts player)-} } player), [{-UpdateThoughts (thoughts player) (temporaryId player)-}])



handleSkillInitiation : Game -> Nat -> (Game, List ClientUpdate)

handleSkillSelection : Game -> (List Nat, List Nat, List Nat, List Nat, List Nat, List Nat) -> (Game, List ClientUpdate)


