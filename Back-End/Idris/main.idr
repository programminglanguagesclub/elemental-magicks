module Main

import Prelude.Nat
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


{-Reset used_death_skill, used_counter_skill before auto skill and action of card. -}

record Game where
 constructor MkGame
 round      : Round
 initiative : WhichPlayer
 turnNumber : Nat
 skillHead  : Maybe (Condition, SkillComponent, SkillComponent, SkillComponent)
 skillQueue : List SkillComponent
 deathQueue : List Nat {-The temporary ids of the monster (maybe this should have its own type?)-}
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
  1     0     0     1     1     0
HB HA HA HB HA HB HB HA HB HA SA SB
  0     1     1     0     0     1
HA HB HB HA HB HA HA HB HA HB SB SA


-}

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


relativizeDrawCommand : CardDraw -> Round -> CardDraw
relativizeDrawCommand cardDraw FirstRound = cardDraw
relativizeDrawCommand cardDraw SecondRound = swapDrawCommand cardDraw


maybeRelativizeDrawCommand : Maybe CardDraw -> Round -> Maybe CardDraw
maybeRelativizeDrawCommand (Just cardDraw) FirstRound = Just cardDraw
maybeRelativizeDrawCommand (Just cardDraw) SecondRound = maybeSwapDrawCommand (Just cardDraw)
maybeRelativizeDrawCommand Nothing _ = Nothing


__getNextTurnDraw : Nat -> Nat -> Maybe CardDraw
__getNextTurnDraw x y = let (a,b) = ((toIntegerNat x),(toIntegerNat y)) in
 if (a == 0 && b == 0) then Just AHand
 else Nothing

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

getNextTurnDraw : Game -> Maybe CardDraw
getNextTurnDraw game =
 let (playerA, playerB) = (relativizePlayer game PlayerA (round game), relativizePlayer game PlayerB (round game)) in
 maybeRelativizeDrawCommand (_getNextTurnDraw game playerA playerB) (round game)


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


{-Might actually want to be stepping round here, not game.-}



goToNextPhase : (Game,List ClientUpdate) -> (Game,List ClientUpdate)
goToNextPhase (game,acc) =
 let (retPhase, phaseUpdate) = nextPhase (phase game) in
 let (game', acc') = (record {phase = retPhase} game, acc ++ [phaseUpdate]) in
 case retPhase of
  SpawnPhase => (record {player_A->thoughts = transformThoughts (\x => x + 2) (thoughts (player_A game')),player_B->thoughts = transformThoughts (\x => x + 2) (thoughts (player_B game'))} game', acc' ++ [phaseUpdate])
  


{-HAVE TO GIVE EXTRA THOUGHTS AND SET DEATHSTALE/FRESH SOMEWHERE WHEN TRANSITIONING PHASES

Also have to change the turn count, and possibly decrement soul points depending on what turn it is.
-}







{-sendSpawnToGraveyard : (Game, List ClientUpdate) -> WhichPlayer -> (Game, List ClientUpdate)-}


getTokens : Game -> WhichPlayer -> (String,String)


spendThoughts : (Game,List ClientUpdate) -> WhichPlayer -> Nat -> (Game,List ClientUpdate)


handleSkillInitiation : Game -> Nat -> (Game, List ClientUpdate)



handleSkillSelection : Game -> (List Nat, List Nat, List Nat, List Nat, List Nat, List Nat) -> (Game, List ClientUpdate)

damageSoul : Game -> Player -> (damage : Nat) -> (Game, List ClientUpdate)

getMonsterField : Player -> Player -> Nat -> Maybe (Player,(Fin 9),Monster)

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
  | (round,initiative,turnNumber,player_A,player_B,DrawPhase,asp,bsp) with (getNextTurnDraw g)
   | Just AHand                                                             = (g, acc ++ [RequestDrawHand (getTokens g PlayerA)])
   | Just BHand                                                             = (g, acc ++ [RequestDrawHand (getTokens g PlayerB)])
   | Just ASoul                                                             = (g, acc ++ [RequestDrawSoul (getTokens g PlayerA)])
   | Just BSoul                                                             = (g, acc ++ [RequestDrawSoul (getTokens g PlayerB)])
   | Nothing                                                                = (g, acc ++ [GameLogicError]) {-(g,acc) {-send message-}-}
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
   | (deadMonster :: deadMonsters) with (getMonsterField player_A player_B deadMonster)
    | Nothing                                                               = (g, acc ++ [GameLogicError])
    | Just (player,location,monster) with (aliveness (basic monster))
     | Alive                                                                = (g, acc ++ [GameLogicError])
     | DeadFresh                                                            = stepGame (record {deathQueue = deadMonsters} g, acc)
     | DeadStale with (level (basic monster))
      |(_,_,baseLevel)                                                      = ?g  {-move card to graveyard, restore thoughts, and then remove life point... -}
   {-
(record {player->board = moveUnit moveFrom moveTo (board (player game))} game)
-}
  | (round,initiative,turnNumber,player_A,player_B,StartPhase,asp,bsp)      = ?g
  | (round,initiative,turnNumber,player_A,player_B,EngagementPhase,asp,bsp) = ?g {-represent going to the next phase if no skills pending, etc, no units disengaged-}
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






_engagementOnMove : (playerABoard : Board) -> (playerBBoard : Board) -> (initiative : WhichPlayer) -> (WhichPlayer, Fin 9)
engagementOnMove : (game : Game) -> (player : Player) -> (opponent : Player) -> (Bool, Fin 9) {-could return a maybe nat, where nothing indicates an error, but I'll trust the ability to not have it the engagement phase if there's nothing next to move-}

_getEnemyStuffInFront : (defenderBoard) -> (row : Fin 3) -> Nat
_getFriendlyStuffInFront : (attackerBoard : Board) -> (attackerSquare : Fin 9) -> Nat 
inRangeRow : (attackerBoard : Board) -> (defenderBoard : Board) -> (attackerSquare : Fin 9) -> (row : Fin 3) -> Maybe Bool
inRangeRow attackerBoard defenderBoard attackerSquare row with (index attackerSquare attackerBoard)
 | Nothing = Nothing
 | Just monster with (aliveness (basic monster))
  | DeadFresh = Nothing
  | DeadStale = Nothing
  | Alive with (range (basic monster))
   | (temporaryRange,_,_) = if gt (fromIntegerNat (extractBounded temporaryRange)) ((_getFriendlyStuffInFront attackerBoard attackerSquare) + (_getEnemyStuffInFront defenderBoard row)) then Just True else Just False


{-
playerOnMove : Game -> Player -> Bool {-assumes engagement phase.. could encode that at type level I suppose-}
-}


moveUnit : (moveFrom : Fin 9) -> (moveTo : Fin 9) -> (board : Board) -> Board {-this actually does a swap-}
moveUnit moveFrom moveTo board = let to = index moveTo board in replaceAt moveFrom to (replaceAt moveTo (index moveFrom board) board)


restUnit : (location : Fin 9) -> Game -> Player {-WhichPlayer-} -> (Game,List ClientUpdate)


_getHandCards : (hand : List Card) -> (acc : MultiTree Nat) -> MultiTree Nat
_getHandCards [] acc = acc
_getHandCards (card::cards) acc with (card)
 |MonsterCard m      = _getHandCards cards (insert acc (permanentId (basic m)))
 |SpellCard s        = _getHandCards cards acc
getHandCards : (hand : List Card) -> MultiTree Nat
getHandCards hand = _getHandCards hand Leaf
__canRevive : (thoughtCost : Nat) -> (thoughts : Thoughts) -> (boardCards : MultiTree Nat) -> (handCards : MultiTree Nat) -> Bool
__canRevive thoughtCost thoughts boardCards handCards = ((extractBoundedNat thoughts) >= thoughtCost) && (dominates handCards boardCards)
_canRevive : Player -> List Bool -> (currentIndex : Nat) -> (thoughtAcc : Nat) -> (boardCardsAcc : MultiTree Nat) -> Bool
_canRevive player [] currentIndex thoughtAcc boardCardsAcc = __canRevive thoughtAcc (thoughts player) boardCardsAcc (getHandCards (hand player))
_canRevive player (x::xs) currentIndex thoughtAcc boardCardsAcc with (x)
 |False             = _canRevive player xs (currentIndex + 1) thoughtAcc boardCardsAcc
 |True with (index' currentIndex (toList (board player)))
  |Nothing          = False {-error-} {-there could be one of two errors: index out of bounds or no monster at location-}
  |Just Nothing     = False
  |Just (Just m) with (aliveness (basic m))
   | Alive          = False
   | DeadFresh      = False {-error-}
   | DeadStale      = _canRevive player xs (currentIndex + 1) (thoughtAcc + (getNumberOfSchools m)) (insert boardCardsAcc (permanentId (basic m)))
canRevive : Player -> Vect 9 Bool -> Bool {-might want to return the cards from the hand as well and the thoughts-}
canRevive player selection = _canRevive player (toList selection) 0 0 Leaf


{-
Rewrite the above code using list comprehension (or at least filter) + fold.

-}



{-might want to refactor this type into a binary datatype and a server update so that I don't have a fail case that I already ruled out... (no player with that token)-}
{-transformGame : Game -> ServerUpdateWrapper -> (Game, List ClientUpdate)-}
transformGame : Game -> (player : Player) -> (opponent : Player) -> WhichPlayer -> ServerUpdate -> (Game, List ClientUpdate)
transformGame game player opponent whichPlayer serverUpdate with (phase game,serverUpdate)
 | (DrawPhase,DrawCard id)                = ?hole {-(game,[])-} {-Maybe-}
 | (DrawPhase,_)                          = (game, [(InvalidMove (token player))])
 | (SpawnPhase,SetCard schools cardIndex) = ?hole
 | (SpawnPhase,Skip schools)              = if (dominatesVect (knowledge player) schools) && (totalDifferenceVect (knowledge player) schools <= extractBounded (thoughts player)) {-Does not yet check to make sure 9 9 9 9 9 9 dominates schools.-}
                                             then ?g
                                             else ?g
 | (SpawnPhase,_)                         = (game, [InvalidMove (token player)])
 | (SpellPhase,SkillSelection s)          = handleSkillSelection game s
 | (SpellPhase,_)                         = (game, [InvalidMove (token player)])
 | (RemovalPhase,SkillSelection s)        = handleSkillSelection game s
 | (RemovalPhase,_)                       = (game, [InvalidMove (token player)])
 | (StartPhase,SkillSelection s)          = handleSkillSelection game s
 | (StartPhase,_)                         = (game, [InvalidMove (token player)])
 | (EngagementPhase,AttackRow n) with (engagementOnMove game player opponent)
  | (False,_)                             = (game, [GameLogicError]) {-assume we already filter for whose turn it is in Ur/Web-}
  | (True,i) with (inRangeRow (board player) (board opponent) i n)
   | Nothing                              = (game, [GameLogicError])
   | Just False                           = (game, [InvalidMove (token player)])
   | Just True                            = ?g {-NEED TO ALSO MAKE SURE THAT THERE IS A VALID TARGET IN THE ROW (NOT JUST THAT IT IS IN RANGE)-}
 | (EngagementPhase,Rest) with (skillHead game, skillQueue game)
  | (Nothing, []) with (engagementOnMove game player opponent)
   | (False,_)                            = (game, [GameLogicError])
   | (True,location)                      = restUnit location game player
  | (_,_)                                 = (game, [InvalidMove(token player)])
 | (EngagementPhase,DirectAttack) with (skillHead game, skillQueue game)
  | (Nothing, []) with (allUnitsDead (board opponent))
   | False                                = (game, [InvalidMove (token player)])
   | True                                 = ?g {-with (engagementOnMove (board))      ... Need to make sure that the user can move with the card that is attacking, and that they have at least 1 thought...-}
  | (_,_)                                 = (game, [InvalidMove (token player)])                 {-   ?hole {-how do I get the player. Should that be passed instead of just the token? Also call all units dead.-}-}
 | (EngagementPhase,Move moveTo) with (engagementOnMove game player opponent)
  | (False,_)                             = (game, [GameLogicError]) {-assume we already filter for whose turn it is in Ur/Web-}
  | (True,moveFrom) with (index moveTo(board (player)))
   | Nothing with (whichPlayer)
    | PlayerA = ((record {player_A->board = moveUnit moveFrom moveTo (board player)} game), [MoveUnit moveFrom moveTo (token player) (token opponent)])
    | PlayerB = ((record {player_B->board = moveUnit moveFrom moveTo (board player)} game), [MoveUnit moveFrom moveTo (token player) (token opponent)])
   | Just monster                         = (game, [InvalidMove (token player)]) {-Can't move to a location with something-}
 | (EngagementPhase,SkillInitiation n)    = handleSkillInitiation game n
 | (EngagementPhase,SkillSelection s)     = handleSkillSelection game s
 | (EngagementPhase,_)                    = (game, [InvalidMove (token player)])
 | (EndPhase,SkillSelection s)            = handleSkillSelection game s {-In all of these handleSkillSelection and handleSkillInitiation have to make sure that the right player is moving, that there is/isn't a pending skill, etc.-}
 | (EndPhase,_)                           = (game, [InvalidMove (token player)])
 | (RevivalPhase,Revive b)                = if canRevive player b
                                             then ?g
                                             else (game, [InvalidMove (token player)])
 | (RevivalPhase,_)                       = (game, [InvalidMove (token player)])


getPlayerByToken : String -> Game -> Maybe Player
getPlayerByToken playerToken game = if (token (player_A game)) == playerToken then Just (player_A game) else if (token (player_B game)) == playerToken then Just (player_B game) else Nothing


{-
while_loop : List Game -> ServerUpdateWrapper -> (List Game, List ClientUpdate) {-ClientUpdate or ClientUpdateWrapper?-}
while_loop [] _      = ?hole {-([],[])-}
while_loop (g::gs) (playerToken, serverUpdate) with (getPlayerByToken playerToken g)
 | Nothing = let (gs',cus) = while_loop gs (playerToken, serverUpdate) in (g::gs',cus)
 | Just player = let (g',cus) = transformGame g player serverUpdate in (g'::gs, cus)


Need to get opponent to call transformGame

-}




{- = if (token (player_A g)) == player_token || (token (player_B g)) == player_token then let (g',cus) = transformGame g (player_token, serverUpdate) in (g'::gs, cus)
                                                  else let (gs',cus) = while_loop gs (player_token, serverUpdate) in (g::gs',cus) -}



main : IO ()
main = do {
x <- reader;
writer (x ++ " was received via Idris, the god of languages. This game is ready to be built in god mode!");
main;
}





{-units now should become engaged AFTER their skills finish (if that's not too hard) actually that might be too hard...-}





