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


record Game where
 constructor MkGame
 round : Bounded 0 1
 player_A_Initiative : Bool
 turnNumber : Nat
 skillHead : Maybe (Condition, SkillComponent, SkillComponent, SkillComponent)
 skillQueue : List SkillComponent
 player_A : Player
 player_B : Player
 phase : Phase
syntax "new" "game" [tokenA] [tokenB] = MkGame (0 ** Oh) True 0 (Vect.Nil,Vect.Nil,Vect.Nil) Nothing [] (new player tokenA) (new player tokenB) DrawPhase

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

discardUsedSpell : Game -> (Game, List ClientUpdate)
discardUsedSpell game = ?hole




loadSkill : Game -> (SkillComponent, Bool, Nat) -> (Game, List ClientUpdate)
loadSkill game = ?hole




{-NEED to use ServerUpdateWrapper (or something that represents the player) rather than just ServerUpdate.-}



{-A lot of the cases for the working with the spawn skills currently work in progress-}

{-Also ignoring updates of the form "It's your turn". Perhaps separate code for this... -}
stepGame : (Game,List ClientUpdate) -> (Game,List ClientUpdate)
stepGame (g,acc) with (skillHead g, skillQueue g)
 | (Just (condition, ifSelects, SkillComponent_ (cannotSelectEffects, cannotSelectSkillHead) , next), skillQueue)

                                                                                     = if skillSelectionPossible g condition then (g,acc)
                                                                                       else let effectsApplied = executeSkillEffects (record {skillHead = cannotSelectSkillHead} g) cannotSelectEffects in
                                                                                            let continue = stepGame (fst effectsApplied, []) in (fst continue, acc ++ (snd effectsApplied) ++ (snd continue))
 | (Nothing, (SkillComponent_ (skillEffects, next))::skillQueue)                     = let effectsApplied = executeSkillEffects (record {skillHead = next, skillQueue = skillQueue} g) skillEffects in
                                                                                       let continue = stepGame (fst effectsApplied, []) in (fst continue, acc ++ (snd effectsApplied) ++ (snd continue))
 | (Nothing, []) with (round g, player_A_Initiative g, turnNumber g, player_A g, player_B g, phase g, getSoulPoints (player_A g), getSoulPoints (player_B g))
  | (round,player_A_Initiative,turnNumber,player_A,player_B,phase,Z,bsp)             = (g, acc ++ [RoundTerminated])
  | (round,player_A_Initiative,turnNumber,player_A,player_B,phase,asp,Z)             = (g, acc ++ [RoundTerminated])
  | (round,player_A_Initiative,turnNumber,player_A,player_B,phase,Z,Z)               = (g, acc ++ [GameLogicError])
  | (round,player_A_Initiative,turnNumber,player_A,player_B,DrawPhase,asp,bsp)       = (g,acc)
  | (round,player_A_Initiative,turnNumber,player_A,player_B,SpawnPhase,asp,bsp)      = (g,acc)
  | (round,player_A_Initiative,turnNumber,player_A,player_B,SpellPhase,asp,bsp) with (spawn player_A, spawn player_B)
   | (Just (MonsterCard cardA),Just (MonsterCard cardB)) with (spawnSkill cardA, spawnSkill cardB)
    |(Just (skillA, usedA, costA),Just (skillB, usedB, costB))                       = ?g
    |(Just (skillA, usedA, costA),Nothing)                                           = ?g
    |(Nothing, Just (skillB, usedB, costB))                                          = ?g
    |(Nothing,Nothing)                                                               = stepGame (record {phase = RemovalPhase} g, acc ++ [SpellPhaseToRemovalPhase])
   | (Just (SpellCard cardA),  Just (MonsterCard cardB)) with (spawnSkill cardA)
    | (skillA, usedA, costA)                                                         = ?g {-if usedA || costA > (fromIntegerNat (extractBounded (thoughts (player_A g)))) then let (g', cu) = discardUsedSpell g in stepGame(g', acc ++ cu) else  ?hole -}
   | (Just (MonsterCard cardA),Just (SpellCard cardB))   with (spawnSkill cardA, spawnSkill cardB)
    | (Just (skillA, usedA, costA), (skillB, usedB, costB))                          = ?g
    | (Nothing, (skillB, usedB, costB))                                              = ?g
   | (Just (SpellCard cardA),  Just (SpellCard cardB))   with (spawnSkill cardA, spawnSkill cardB)
    | ((skillA, usedA, costA), (skillB, usedB, costB))                               = ?g
   | (Just (MonsterCard cardA),Nothing)                  with (spawnSkill cardA)
    | Just (skillA, usedA, costA)                                                    = ?g
    | Nothing                                                                        = ?g
   | (Just (SpellCard cardA),  Nothing)                                              = ?g
   | (Nothing,                 Just (MonsterCard cardB)) with (spawnSkill cardB)
    | Just (skillB, usedB, costB)                                                    = ?g {-let (g', acc') = killFatallyDamaged (g, acc) in stepGame (record {phase = RemovalPhase} g, acc' ++ [SpellPhaseToRemovalPhase]) -}
    | Nothing                                                                        = ?g
   | (Nothing,                 Just (SpellCard cardB))   with (spawnSkill cardB)
    | (skillB, usedB, costB)                                                         = ?g
   | (Nothing, Nothing)                                                              = stepGame (record {phase = RemovalPhase} g, acc ++ [SpellPhaseToRemovalPhase])
  | (round,player_A_Initiative,turnNumber,player_A,player_B,RemovalPhase,asp,bsp)    = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,StartPhase,asp,bsp)      = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,EngagementPhase,asp,bsp) = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,EndPhase,asp,bsp)        = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,RevivalPhase,asp,bsp)    = ?g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,DeploymentPhase,asp,bsp) = (g,acc)

{-Need to cause units to leave the field if not revived in order of death, and then in order of position on the field. For this we need another data structure in game to represent the order of death-}

{-For now, completely ignore the possibility of the user using skills! :D -}




_allUnitsDead : List (Maybe Monster) -> Bool
_allUnitsDead (Nothing::tl) = _allUnitsDead tl
_allUnitsDead ((Just m)::tl) with (aliveness (basic m))
 | Alive = False
 | DeadFresh = _allUnitsDead tl
 | DeadStale = _allUnitsDead tl
_allUnitsDead [] = True


allUnitsDead : Vect n (Maybe Monster) -> Bool
allUnitsDead board = _allUnitsDead (toList board)

playerOnMove : Game -> Player -> Bool {-assumes engagement phase.. could encode that at type level I suppose-}



{-
this code wrong in many ways.
allUnitsDead : (n : Nat) -> Vect n (Maybe Monster) -> Bool
allUnitsDead n (Nothing::tl) = allUnitsDead (n-1) tl
allUnitsDead n ((Just m)::tl) = False
allUnitsDead n [] = True
-}

{-might want to refactor this type into a binary datatype and a server update so that I don't have a fail case that I already ruled out... (no player with that token)-}
transformGame : Game -> ServerUpdateWrapper -> (Game, List ClientUpdate)
transformGame game serverUpdateWrapper with (phase game,serverUpdateWrapper)
 | (DrawPhase,(player_token, DrawCard id))                = ?hole {-(game,[])-} {-Maybe-}
 | (DrawPhase,_)                                          = (game, [InvalidMove])
 | (SpawnPhase,(player_token, SetCard schools cardIndex)) = ?hole
 | (SpawnPhase,(player_token, Skip schools))              = ?hole
 | (SpawnPhase,_)                                         = (game, [InvalidMove])
 | (SpellPhase,(player_token, SkillSelection n))          = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (SpellPhase,_)                                         = (game, [InvalidMove])
 | (RemovalPhase,(player_token, SkillSelection n))        = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (RemovalPhase,_)                                       = (game, [InvalidMove])
 | (StartPhase,(player_token, SkillSelection n))          = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (StartPhase,_)                                         = (game, [InvalidMove])
 | (EngagementPhase, (player_token, AttackRow n))         = ?hole
 | (EngagementPhase, (player_token, Rest))                = ?hole
 | (EngagementPhase, (player_token, DirectAttack))        = ?hole {-how do I get the player. Should that be passed instead of just the token? Also call all units dead.-}
 | (EngagementPhase, (player_token, Move))                = ?hole
 | (EngagementPhase, (player_token, SkillInitiation n))   = ?hole
 | (EngagementPhase, (player_token, SkillSelection n))    = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (EngagementPhase,_)                                    = (game, [InvalidMove])
 | (EndPhase,(player_token, SkillSelection n))            = ?hole {-again, this (the n) is currently indexed incorrectly-}
 | (EndPhase,_)                                           = (game, [InvalidMove])
 | (RevivalPhase,(player_token, Revive b))                = ?hole
 | (RevivalPhase,_)                                       = (game, [InvalidMove])


while_loop : List Game -> ServerUpdateWrapper -> (List Game, List ClientUpdate) {-ClientUpdate or ClientUpdateWrapper?-}
while_loop [] _      = ?hole {-([],[])-}
while_loop (g::gs) (player_token, serverUpdate) = if (token (player_A g)) == player_token || (token (player_B g)) == player_token then let (g',cus) = transformGame g (player_token, serverUpdate) in (g'::gs, cus)
                                                  else let (gs',cus) = while_loop gs (player_token, serverUpdate) in (g::gs',cus)




main : IO ()
main = do {
x <- reader;
writer (x ++ " was received via Idris, the god of languages. This game is ready to be built in god mode!");
main;
}





{-units now should become engaged AFTER their skills finish (if that's not too hard) actually that might be too hard...-}





