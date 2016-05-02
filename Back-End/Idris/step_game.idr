module Step_game

import Data.Fin
import Data.So
import preliminaries
import phase
import objects_basic
import skill_dsl
import objects_advanced
import serverupdates
import clientupdates
import step_game_helpers

public export
stepGame : (Game,List ClientUpdate) -> (Game,List ClientUpdate)
stepGame (g,acc) with (skillHead g, skillQueue g)
 | (Just (condition, ifSelects, SkillComponent_ (cannotSelectEffects, cannotSelectSkillHead) , next), skillQueue)
                                                                            = if skillSelectionPossible g condition then (g,acc)
                                                                              else let effectsApplied = executeSkillEffects (record {skillHead = cannotSelectSkillHead} g) cannotSelectEffects in
                                                                                   let continue = stepGame (fst effectsApplied, []) in (fst continue, acc ++ (snd effectsApplied) ++ (snd continue))
 | (Nothing, (SkillComponent_ (skillEffects, next))::skillQueue)            = let effectsApplied = executeSkillEffects (record {skillHead = next, skillQueue = skillQueue} g) skillEffects in
                                                                              let continue = stepGame (fst effectsApplied, []) in (fst continue, acc ++ (snd effectsApplied) ++ (snd continue))
 | (Nothing, []) with (initiative g, turnNumber g, player_A g, player_B g, phase g, getSoulPoints (player_A g), getSoulPoints (player_B g))
  | (initiative,turnNumber,player_A,player_B,phase,Z,bsp)             = (g, acc ++ [RoundTerminated])
  | (initiative,turnNumber,player_A,player_B,phase,asp,Z)             = (g, acc ++ [RoundTerminated])
  | (initiative,turnNumber,player_A,player_B,phase,Z,Z)               = (g, acc ++ [GameLogicError])
  | (initiative,turnNumber,player_A,player_B,DrawPhase,asp,bsp) with (getNextTurnDraw g)
   | Just AHand                                                             = (g, acc ++ [RequestDrawHand (getTokens g PlayerA)])
   | Just BHand                                                             = (g, acc ++ [RequestDrawHand (getTokens g PlayerB)])
   | Just ASoul                                                             = (g, acc ++ [RequestDrawSoul (getTokens g PlayerA)])
   | Just BSoul                                                             = (g, acc ++ [RequestDrawSoul (getTokens g PlayerB)])
   | Nothing                                                                = (g, acc ++ [GameLogicError]) {-(g,acc) {-send message-}-}
  | (initiative,turnNumber,player_A,player_B,SpawnPhase,asp,bsp)      = (g,acc) {-send message-}
  | (initiative,turnNumber,player_A,player_B,SpellPhase,asp,bsp) with (spawn player_A, spawn player_B)
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
  | (initiative,turnNumber,player_A_,player_B_,RemovalPhase,asp,bsp) with (deathQueue g)
   | []                                                                     = stepGame (goToNextPhase (g,acc))
   | (deadMonster :: deadMonsters) with (getMonsterField player_A_ player_B_ deadMonster)
    | Nothing                                                               = (g, acc ++ [GameLogicError])
    | Just (player,location,monster) with (aliveness (basic monster))
     | Alive                                                                = (g, acc ++ [GameLogicError])
     | DeadFresh                                                            = stepGame (record {deathQueue = deadMonsters} g, acc)
     | DeadStale with (level (basic monster))
      |(_,_,baseLevel)                                                      = if (token player_A_) == (token player) {-move card to graveyard, restore thoughts, and then remove life point... DOES NOT DO ALL OF THIS YET. STILL WORKING-}
                                                                               then
                                                                                let g' = (record {player_A->thoughts = transformThoughts (\x => x + (extractBounded baseLevel)) (thoughts player_A_),
                                                                                                  player_A->graveyard = ((graveyard player_A_) ++ [MonsterCard monster])} g) in ?g


{-damageSoul (?g,acc ++ [UpdateThoughts ]) player_A (S Z)-}
                                                                               else ?g


   {-
UpdateThoughts Nat String String
damageSoul : (Game, List ClientUpdate) -> Player -> (damage : Nat) -> (Game, List ClientUpdate)

(record {player->board = moveUnit moveFrom moveTo (board (player game))} game)
-}
  | (initiative,turnNumber,player_A,player_B,StartPhase,asp,bsp)      = ?g
  | (initiative,turnNumber,player_A,player_B,EngagementPhase,asp,bsp) = ?g {-represent going to the next phase if no skills pending, etc, no units disengaged-}
  | (initiative,turnNumber,player_A,player_B,EndPhase,asp,bsp)        = ?g
  | (initiative,turnNumber,player_A,player_B,RevivalPhase,asp,bsp)    = ?g
  | (initiative,turnNumber,player_A,player_B,DeploymentPhase,asp,bsp) with (spawn player_A, spawn player_B)
   | (Nothing, Nothing)                                                     = stepGame (goToNextPhase (g,acc))
   | (Just (MonsterCard cardA), Nothing)                                    = if boardFull (board player_A) then ?g else (g, acc ++ [DeployCardRequest (token player_A)])
   | (Nothing, Just(MonsterCard cardB))                                     = if boardFull (board player_B) then ?g else (g, acc ++ [DeployCardRequest (token player_B)])
   | (Just (MonsterCard cardA), Just (MonsterCard cardB))                   = if boardFull (board player_A) then ?g else if boardFull (board player_B) then ?g
                                                                              else (g, acc ++ [DeployCardRequest (token (getPlayer g initiative))])
   | (Just (SpellCard cardA),_)                                             = (g, acc ++ [GameLogicError])
   | (_,Just (SpellCard cardB))                                             = (g, acc ++ [GameLogicError])
