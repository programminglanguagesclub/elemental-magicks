module Main



import Data.Vect
import Data.Fin
import Data.So
import preliminaries
import phase
import objects_basic
import skill_dsl
import objects_advanced
import serverupdates
import clientupdates



%include C "../Glue/reader.h"
%link C "../Glue/reader.o"

reader : IO String
reader = foreign FFI_C "reader" (IO String)

writer : String -> IO Unit
writer x = foreign FFI_C "writer" (String -> IO Unit) x

data WhichPlayer = PlayerA | PlayerB
data Round = FirstRound | SecondRound

record Game where
 constructor MkGame
 round      : Round
 initiative : WhichPlayer
 turnNumber : Nat
 skillHead  : Maybe (Condition, SkillComponent, SkillComponent, SkillComponent)
 skillQueue : List SkillComponent
 deathQueue : List Monster
 player_A   : Player
 player_B   : Player
 phase      : Phase
 
syntax "new" "game" [tokenA] [tokenB] = MkGame (0 ** Oh) PlayerA 0 (Vect.Nil,Vect.Nil,Vect.Nil) Nothing [] [] (new player tokenA) (new player tokenB) DrawPhase






getPlayer : Game -> WhichPlayer -> Player
getPlayer game PlayerA = player_A game
getPlayer game PlayerB = player_B game

opponent : WhichPlayer -> WhichPlayer
opponent PlayerA = PlayerB
opponent PlayerB = PlayerA


relativizePlayer : Game -> WhichPlayer -> Round -> Player
relativizePlayer game p FirstRound = getPlayer game p
relativizePlayer game p SecondRound = getPlayer game (opponent p)

{-



  0     1     1     0     0     1
HA HB HB HA HB HA HA HB HA HB SB SA
  1     0     0     1     1     0
HB HA HA HB HA HB HB HA HB HA SA SB
  0     1     1     0     0     1
HA HB HB HA HB HA HA HB HA HB SB SA
HB HA HA HB HA HB HB HA HB HA SA SB
HA HB HB HA HB HA HA HB HA HB SB SA


-}

data CardDraw = AHand | BHand | ASoul | BSoul

{-Do I have to inverse the action of relativize player when I return to  getNextTurnDraw?-}

_getNextTurnDraw : Game -> Player -> Player -> WhichPlayer
_getNextTurnDraw game playerA playerB with (length (hand playerA),length (hand playerB))
 | (0,0)
 | (1,0)
 | (1,1)
 | (1,2)
 | (2,2)
 | (2,3)
 | (3,3)
 | (4,3)
 | (4,4)
 | (5,4)
 | (5,5)
 | (5,6)
 | (6,6)



_getNextTurnDraw : Game -> Player -> Player -> WhichPlayer
_getNextTurnDraw game playerA playerB with (length (hand playerA),length (hand playerB))






getNextTurnDraw : Game -> WhichPlayer
getNextTurnDraw game = let (playerA, playerB) = (relativizePlayer game PlayerA (round game), relativizePlayer game PlayerB (round game)) in _getNextTurnDraw game playerA playerB


Selection : {b : Nat} -> {h : Nat} -> {g : Nat} -> (game : Game) -> (Vect b BoardIndex, Vect h HandIndex, Vect g GraveyardIndex) -> (Game, List ClientUpdate)
Selection game (board, hand, graveyard) with (skillHead game)
 | Nothing = ?hole {-(game, [])-}
 | skill = ?hole

getSoulPoints : Player -> Nat
getSoulPoints player = ?hole

{-
FooDrawCard : Player n m -> Card -> Player (S n) m
FooDrawCard player card = MkPlayer (board player) (reverse (card :: (reverse (hand player)))) (graveyard player) (spawn player) (soul player) (thoughts player) (knowledge player) (token player)
-}


killFatallyDamaged : (Game, List ClientUpdate) -> (Game, List ClientUpdate)



executeSkillEffects : Game -> List SkillEffect -> (Game, List ClientUpdate)
executeSkillEffects g a = ?hole

skillSelectionPossible : Game -> Condition -> Bool
skillSelectionPossible game condition = ?hole



{-discardUsedSpell : Game -> (Game, List ClientUpdate)
discardUsedSpell game = ?hole
-}

removeSpawnFromGame : (Game, List ClientUpdate) -> WhichPlayer -> (Game, List ClientUpdate)
removeSpawnFromGame (game, acc) PlayerA  with (spawn (player_A game))
 | Nothing = (game, acc ++ [GameLogicError])
 | Just card = (record {player_A -> discard = (discard (player_A game)) ++ [card], player_A -> spawn = Nothing} game, acc ++ [SendSpawnToDiscard (token (player_A game)) (token (player_B game))])
removeSpawnFromGame (game, acc) PlayerB with (spawn (player_B game))
 | Nothing = (game, acc ++ [GameLogicError])
 | Just card = (record {player_B -> discard = (discard (player_B game)) ++ [card], player_B -> spawn = Nothing} game, acc ++ [SendSpawnToDiscard (token (player_B game)) (token (player_A game))])



loadSkill : Game -> (SkillComponent, Bool, Nat) -> (Game, List ClientUpdate)
loadSkill game = ?hole



_boardFull : List (Maybe Monster) -> Bool
_boardFull (Nothing::_) = False
_boardFull ((Just m)::tl) = _boardFull tl
_boardFull [] = True

boardFull : Board -> Bool {-don't want to make players try to deploy if the board is full -}
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

{-NEED to use ServerUpdateWrapper (or something that represents the player) rather than just ServerUpdate.-}


{-Might actually want to be stepping round here, not game.-}



{-A lot of the cases for the working with the spawn skills currently work in progress-}

{-Also ignoring updates of the form "It's your turn". Perhaps separate code for this... -}


goToNextPhase : (Game,List ClientUpdate) -> (Game,List ClientUpdate)
goToNextPhase (game,acc) = let (retPhase, phaseUpdate) = nextPhase (phase game) in (record {phase = retPhase} game, acc ++ [phaseUpdate])

{-sendSpawnToGraveyard : (Game, List ClientUpdate) -> WhichPlayer -> (Game, List ClientUpdate)-}



stepGame : (Game,List ClientUpdate) -> (Game,List ClientUpdate)
stepGame (g,acc) with (skillHead g, skillQueue g)
 | (Just (condition, ifSelects, SkillComponent_ (cannotSelectEffects, cannotSelectSkillHead) , next), skillQueue)
                                                                            = if skillSelectionPossible g condition then (g,acc)
                                                                              else let effectsApplied = executeSkillEffects (record {skillHead = cannotSelectSkillHead} g) cannotSelectEffects in
                                                                                   let continue = stepGame (fst effectsApplied, []) in (fst continue, acc ++ (snd effectsApplied) ++ (snd continue))
 | (Nothing, (SkillComponent_ (skillEffects, next))::skillQueue)            = let effectsApplied = executeSkillEffects (record {skillHead = next, skillQueue = skillQueue} g) skillEffects in
                                                                              let continue = stepGame (fst effectsApplied, []) in (fst continue, acc ++ (snd effectsApplied) ++ (snd continue))
 | (Nothing, []) with (round g, initiative g, turnNumber g, player_A g, player_B g, phase g, getSoulPoints (player_A g), getSoulPoints (player_B g))
  | (round,initiative,turnNumber,player_A,player_B,phase,Z,bsp)             = (g, acc ++ [RoundTerminated])
  | (round,initiative,turnNumber,player_A,player_B,phase,asp,Z)             = (g, acc ++ [RoundTerminated])
  | (round,initiative,turnNumber,player_A,player_B,phase,Z,Z)               = (g, acc ++ [GameLogicError])
  | (round,initiative,turnNumber,player_A,player_B,DrawPhase,asp,bsp)       = (g,acc) {-send message-}
  | (round,initiative,turnNumber,player_A,player_B,SpawnPhase,asp,bsp)      = (g,acc) {-send message-}
  | (round,initiative,turnNumber,player_A,player_B,SpellPhase,asp,bsp) with (spawn player_A, spawn player_B)
   | (Just (MonsterCard cardA),Just (MonsterCard cardB)) with (spawnSkill cardA, spawnSkill cardB)
    |(Just (skillA, usedA, costA),Just (skillB, usedB, costB))              = ?g
    |(Just (skillA, usedA, costA),Nothing)                                  = ?g
    |(Nothing, Just (skillB, usedB, costB))                                 = ?g
    |(Nothing,Nothing)                                                      = stepGame (goToNextPhase (g,acc))
   | (Just (SpellCard cardA),  Just (MonsterCard cardB)) with (spawnSkill cardA)
    | (skillA, usedA, costA)                                                = ?g {-if usedA || costA > (fromIntegerNat (extractBounded (thoughts (player_A g)))) then let (g', cu) = discardUsedSpell g in stepGame(g', acc ++ cu) else  ?hole -}
   | (Just (MonsterCard cardA),Just (SpellCard cardB))   with (spawnSkill cardA, spawnSkill cardB)
    | (Just (skillA, usedA, costA), (skillB, usedB, costB))                 = ?g
    | (Nothing, (skillB, usedB, costB))                                     = ?g
   | (Just (SpellCard cardA),  Just (SpellCard cardB))   with (spawnSkill cardA, spawnSkill cardB)
    | ((skillA, usedA, costA), (skillB, usedB, costB))                      = ?g
   | (Just (MonsterCard cardA),Nothing)                  with (spawnSkill cardA)
    | Just (skillA, usedA, costA)                                           = ?g
    | Nothing                                                               = ?g
   | (Just (SpellCard cardA),  Nothing)                                     = ?g
   | (Nothing,                 Just (MonsterCard cardB)) with (spawnSkill cardB)
    | Just (skillB, usedB, costB)                                           = ?g {-let (g', acc') = killFatallyDamaged (g, acc) in stepGame (record {phase = RemovalPhase} g, acc' ++ [SpellPhaseToRemovalPhase]) -}
    | Nothing                                                               = ?g
   | (Nothing,                 Just (SpellCard cardB))   with (spawnSkill cardB)
    | (skillB, usedB, costB)                                                = ?g
   | (Nothing, Nothing)                                                     = stepGame (goToNextPhase (g,acc))
  | (round,initiative,turnNumber,player_A,player_B,RemovalPhase,asp,bsp) with (deathQueue g)
   | []                                                                     = stepGame (goToNextPhase (g,acc))
   | (deadMonster :: deadMonsters)                                          = ?g {-move card to graveyard, restore thoughts, and then remove life point... -}
  | (round,initiative,turnNumber,player_A,player_B,StartPhase,asp,bsp)      = ?g
  | (round,initiative,turnNumber,player_A,player_B,EngagementPhase,asp,bsp) = ?g
  | (round,initiative,turnNumber,player_A,player_B,EndPhase,asp,bsp)        = ?g
  | (round,initiative,turnNumber,player_A,player_B,RevivalPhase,asp,bsp)    = ?g
  | (round,initiative,turnNumber,player_A,player_B,DeploymentPhase,asp,bsp) with (spawn player_A, spawn player_B)
   | (Nothing, Nothing)                                                     = stepGame (goToNextPhase (g,acc))
   | (Just (MonsterCard cardA), Nothing)                                    = if boardFull (board player_A) then ?g else (g, acc ++ [DeployCardRequest (token player_A)])
   | (Nothing, Just(MonsterCard cardB))                                     = if boardFull (board player_B) then ?g else (g, acc ++ [DeployCardRequest (token player_B)])
   | (Just (MonsterCard cardA), Just (MonsterCard cardB))                   = if boardFull (board player_A) then ?g else if boardFull (board player_B) then ?g
                                                                              else (g, acc ++ [DeployCardRequest (token (getPlayer g initiative))])
   | (Just (SpellCard cardA),_)                                             = (g, acc ++ [GameLogicError])
   | (_,Just (SpellCard cardB))                                             = (g, acc ++ [GameLogicError])

{-Need to cause units to leave the field if not revived in order of death, and then in order of position on the field. For this we need another data structure in game to represent the order of death-}

{-For now, completely ignore the possibility of the user using skills! :D -}

playerOnMove : Game -> Player -> Bool {-assumes engagement phase.. could encode that at type level I suppose-}

{-might want to refactor this type into a binary datatype and a server update so that I don't have a fail case that I already ruled out... (no player with that token)-}
{-transformGame : Game -> ServerUpdateWrapper -> (Game, List ClientUpdate)-}
transformGame : Game -> Player -> ServerUpdate -> (Game, List ClientUpdate)
transformGame game player serverUpdate with (phase game,serverUpdate)
 | (DrawPhase,DrawCard id)                = ?hole {-(game,[])-} {-Maybe-}
 | (DrawPhase,_)                          = (game, [InvalidMove])
 | (SpawnPhase,SetCard schools cardIndex) = ?hole
 | (SpawnPhase,Skip schools)              = ?hole
 | (SpawnPhase,_)                         = (game, [InvalidMove])
 | (SpellPhase,SkillSelection n)          = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (SpellPhase,_)                         = (game, [InvalidMove])
 | (RemovalPhase,SkillSelection n)        = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (RemovalPhase,_)                       = (game, [InvalidMove])
 | (StartPhase,SkillSelection n)          = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (StartPhase,_)                         = (game, [InvalidMove])
 | (EngagementPhase,AttackRow n)          = ?hole
 | (EngagementPhase,Rest)                 = ?hole
 | (EngagementPhase,DirectAttack)         = ?hole {-how do I get the player. Should that be passed instead of just the token? Also call all units dead.-}
 | (EngagementPhase,Move)                 = ?hole
 | (EngagementPhase,SkillInitiation n)    = ?hole
 | (EngagementPhase,SkillSelection n)     = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (EngagementPhase,_)                    = (game, [InvalidMove])
 | (EndPhase,SkillSelection n)            = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (EndPhase,_)                           = (game, [InvalidMove])
 | (RevivalPhase,Revive b)                = ?hole
 | (RevivalPhase,_)                       = (game, [InvalidMove])


getPlayerByToken : String -> Game -> Maybe Player
getPlayerByToken playerToken game = if (token (player_A game)) == playerToken then Just (player_A game) else if (token (player_B game)) == playerToken then Just (player_B game) else Nothing

while_loop : List Game -> ServerUpdateWrapper -> (List Game, List ClientUpdate) {-ClientUpdate or ClientUpdateWrapper?-}
while_loop [] _      = ?hole {-([],[])-}
while_loop (g::gs) (playerToken, serverUpdate) with (getPlayerByToken playerToken g)
 | Nothing = let (gs',cus) = while_loop gs (playerToken, serverUpdate) in (g::gs',cus)
 | Just player = let (g',cus) = transformGame g player serverUpdate in (g'::gs, cus)





{- = if (token (player_A g)) == player_token || (token (player_B g)) == player_token then let (g',cus) = transformGame g (player_token, serverUpdate) in (g'::gs, cus)
                                                  else let (gs',cus) = while_loop gs (player_token, serverUpdate) in (g::gs',cus) -}



main : IO ()
main = do {
x <- reader;
writer (x ++ " was received via Idris, the god of languages. This game is ready to be built in god mode!");
main;
}





{-units now should become engaged AFTER their skills finish (if that's not too hard) actually that might be too hard...-}





