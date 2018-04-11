module Main.Step_game_helpers
import Data.Vect
import Data.So
import Base.Bounded
import Base.Preliminaries
import Base.Objects_basic
import Base.Skill_dsl_data
import Base.Player
import Main.Game
import Base.Clientupdates
import Base.Card
%access public export
%default total

-------------------------------------------------------------------------------
-- not responsible for further effects resulting?
damageSoul :
 (Game, List ClientUpdate) ->
 Player ->
 (damage : Nat) ->
 (Maybe Game, List ClientUpdate)

-- What does nothing here mean? The game is over?
damageSoul (game,updates) player damage = ?hole
-------------------------------------------------------------------------------
{-I think some places I used Bounded 0 25 for hand index. Should change that if I did-}

Selection :
 {b : Nat} ->
 {h : Nat} ->
 {g : Nat} ->
 (game : Game) ->
 (Vect b (Fin 9), Vect h (Fin 25), Vect g (Fin 25)) ->
 (Game, List ClientUpdate)

Selection game (board, hand, graveyard) with (skillHead game)
  | _ = ?hole
 {-| Nothing = ?hole {-(game, [])-}{-probably match on whether it's terminated.-}
 | skill = ?hole
 -}
-------------------------------------------------------------------------------


{- SHOULD FOLD HERE -}
getSoulPointsRemainingOnMonster : Monster -> Integer
getSoulPointsRemainingOnMonster m with (soulPoints ( basic(m) ))
  | (MkBounded (current ** _),_) = current
-------------------------------------------------------------------------------
getPointsFromSoul : Vect 5 Monster -> Integer
getPointsFromSoul n =
 foldrImpl (\x,y => getSoulPointsRemainingOnMonster(x)+y) 0 (\x => x) n
-------------------------------------------------------------------------------
getSoulPoints : Player -> Integer
getSoulPoints player = getPointsFromSoul(soulCards player)
-------------------------------------------------------------------------------
{-
FooDrawCard : Player n m -> Card -> Player (S n) m
FooDrawCard player card = MkPlayer (board player) (reverse (card :: (reverse (hand player)))) (graveyard player) (spawn player) (soul player) (thoughts player) (knowledge player) (temporaryId player)
-}



-------------------------------------------------------------------------------
killFatallyDamaged : (Game, List ClientUpdate) -> (Game, List ClientUpdate)
-------------------------------------------------------------------------------

something : Maybe a -> Bool
something Nothing = False
something (Just _) = True

data Something : Maybe a -> Type where
 MkSomething : (x : a) -> Something (Just x)


-------------------------------------------------------------------------------
{-removeSpawnFromGame :
 (Game, List ClientUpdate) ->
 (which : WhichPlayer) ->
 Something (spawnCard (getPlayer which game)) ->
 (Game, List ClientUpdate)
-}


removeSpawnFromGame :
 (Game, List ClientUpdate) ->
 (which : WhichPlayer) ->
 something (spawnCard (getPlayer which game)) = true ->
 (Game, List ClientUpdate)


-- might need decEq for card....

{-
-- assume player a for now
removeSpawnFromGame (game,acc) which prf with (spawnCard (playerA game))
 | Nothing
   with (decEq (something (spawnCard (playerA game))) True)
    | Yes prf1 = ?hole
    | No prf2 = absurd prf2
 | Just card = ?hole
-}

---?hole --(record {player_A -> discard = (discard (player_A game)) ++ [card], player_A -> spawn = Nothing} game, acc ++ [SendSpawnToDiscard (temporaryId (player_A game))])

{-
removeSpawnFromGame (game, acc) PlayerA with (spawnCard (player_A game))
 | Nothing = (game, acc ++ [GameLogicError])
 | Just card = (record {player_A -> discard = (discard (player_A game)) ++ [card], player_A -> spawn = Nothing} game, acc ++ [SendSpawnToDiscard (temporaryId (player_A game))])
removeSpawnFromGame (game, acc) PlayerB with (spawn (player_B game))
 | Nothing = (game, acc ++ [GameLogicError])
 | Just card = (record {player_B -> discard = (discard (player_B game)) ++ [card], player_B -> spawn = Nothing} game, acc ++ [SendSpawnToDiscard (temporaryId (player_B game))])

loadSkill : Game -> (Automatic, Bool, Nat) -> (Game, List ClientUpdate)
loadSkill game = ?hole
-}
-------------------------------------------------------------------------------
resetSkill : Maybe (Skill, SkillUsedness) -> Maybe (Skill, SkillUsedness)
resetSkill Nothing = Nothing
resetSkill (Just (s,_)) = Just (s,Unused)
-------------------------------------------------------------------------------
resetAllSkillsMonster : Monster -> Monster
resetAllSkillsMonster monster =
 record {
  startSkill $= resetSkill,
  endSkill $= resetSkill,
  counterSkill $= resetSkill,
  deathSkill $= resetSkill,
  autoSkill $= resetSkill
 } monster
-------------------------------------------------------------------------------
resetAllSkillsSquare : Maybe Monster -> Maybe Monster
resetAllSkillsSquare square = map resetAllSkillsMonster square
-------------------------------------------------------------------------------
resetAllSkillsBoard : Vect 9 (Maybe Monster) -> Vect 9 (Maybe Monster)
resetAllSkillsBoard board = map resetAllSkillsSquare board
-------------------------------------------------------------------------------
resetAllSkillsPlayer : Player -> Player
resetAllSkillsPlayer player = ?hole
-- currently the player board is a vector of vectors
-- I need to finalize this design.
-------------------------------------------------------------------------------
resetAllSkills : Game -> Game
resetAllSkills game =
 record {
  playerA $= resetAllSkillsPlayer,
  playerB $= resetAllSkillsPlayer
 } game
-------------------------------------------------------------------------------

-- this doesn't generate any client updates because this always happens
-- at the same time and the user doesn't have to be alerted of it.
-- this might not be correct but I'm only doing this for cards that are on the 9 square bord.
{-Resets start skills, end skills, and counter skills. also decrements engagement (autoskills are reset whenever this happens) -}
-------------------------------------------------------------------------------
possiblyDecrementSoul : Game -> (Game, List ClientUpdate) {-oops, this might have to call step game....-}
-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------
{-sendSpawnToGraveyard : (Game, List ClientUpdate) -> WhichPlayer -> (Game, List ClientUpdate)-}
-------------------------------------------------------------------------------
getTemporaryIdentifiers : Game -> WhichPlayer -> (String,String)
-------------------------------------------------------------------------------
{-
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
-}

-------------------------------------------------------------------------------
spendThoughts :
 Player ->
 Integer ->
 (Player, List ClientUpdate)

spendThoughts player val =
 ((record {thoughtsResource = {- (\t => t - val) -} ?hole {-(thoughts player)-} } player),
  [{-UpdateThoughts (thoughts player) (temporaryId player)-}])
-------------------------------------------------------------------------------
handleSkillInitiation : Game -> Nat -> (Game, List ClientUpdate)
-------------------------------------------------------------------------------
handleSkillSelection : Game -> (List Nat, List Nat, List Nat, List Nat, List Nat, List Nat) -> (Game, List ClientUpdate)
-------------------------------------------------------------------------------
