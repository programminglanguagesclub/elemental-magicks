module Main.Step_game

import Data.Fin
import Data.So
import Data.Vect
import Base.Bounded
import Base.Bounded_then_integer
import Base.Integer_then_bounded
import Base.Preliminaries
import Base.Phase
import Base.Objects_basic
import Base.Skill_dsl_data
import Base.Card
import Base.Player
import Main.Game
import Main.Serverupdates
import Base.Clientupdates
import Main.Draw_phase
import Main.Spawn_phase
import Main.Spell_phase
import Main.Removal_phase
import Main.Start_phase
import Main.Engagement_phase
import Main.End_phase
import Main.Revival_phase
import Main.Deployment_phase
import Main.Step_game_helpers
%access public export
%default total


{-THERE'S A HUGE BUG HERE!!!
Normally, I want to finish executing skills before going on, but an exception is applied to the spell phase.
During that phase, I want both spells to finish activating before dealing with any counter skills, etc.

I'm not sure if the code currently does this.





I can achieve this by pushing both of the spawn skills onto the queue/head/whatever at the start of the spell phase (during the transition to the spell phase)

-}
{-  | _ = ?hole {-again, going to have to change to checking if the skillHead is terminated or not, rather than being nothing or just...-}-}


{-(Just (condition, ifSelects, SkillComponent_ (cannotSelectEffects, cannotSelectSkillHead) playerTemporaryId, next), skillQueue)-} 
    {-if skillSelectionPossible g condition
           then (g,acc)
                  else let effectsApplied = executeSkillEffects (record {skillHead = cannotSelectSkillHead} g) cannotSelectEffects in
                                           let continue = stepGame (fst effectsApplied, []) in (fst continue, acc ++ (snd effectsApplied) ++ (snd continue))-}
                                           






{-
transform can have valid : phase -> update -> bool?
                        -}




stepGameNoSkills : (Game, List ClientUpdate) -> (Game, List ClientUpdate) {- assumes that skillHead g and skillQueue g are empty -}
stepGameNoSkills (g,acc) with (phase g)
  | DrawPhase = ?hole
  | SpawnPhase = ?hole
  | SpellPhase = ?hole
  | RemovalPhase = ?hole
  | StartPhase = ?hole
  | EngagementPhase = ?hole
  | EndPhase = ?hole
  | RevivalPhase = ?hole
  | DeploymentPhase = ?hole



stepGame : (Game,List ClientUpdate) -> (Game,List ClientUpdate)
stepGame (g,acc) with (skillHead g, skillQueue g)
  | (TerminatedSkillComponent, []) = stepGameNoSkills (g,acc)
  | (TerminatedSkillComponent, (pendingSkill::pendingSkills)) = ?hole {-stepGame (record {skillHead = pendingSkill, skillQueue = pendingSkills} g,acc) -}{-wrong type... need to execute head first... -}
  | _ = ?hole 


{-
stepGame (g,acc) with (skillHead g, skillQueue g)
 | (Nothing, (SkillComponent_ (skillEffects, next) playerTemporaryId)::skillQueue) =
   let effectsApplied = executeSkillEffects (record {skillHead = next, skillQueue = skillQueue} g) skillEffects in
       let continue = stepGame (fst effectsApplied, []) in (fst continue, acc ++ (snd effectsApplied) ++ (snd continue))
 | (Nothing, []) with (initiative g, turnNumber g, player_A g, player_B g, phase g, getSoulPoints (player_A g), getSoulPoints (player_B g))
  | (initiative,turnNumber,player_A,player_B,phase,0,bsp) = (g, acc ++ [RoundTerminated])
  | (initiative,turnNumber,player_A,player_B,phase,asp,0) = (g, acc ++ [RoundTerminated])
  | (initiative,turnNumber,player_A,player_B,phase,0,0) = (g, acc ++ [GameLogicError]) {- this case cannot be reached... -}
  | (initiative,turnNumber,player_A,player_B,DrawPhase,asp,bsp) with (getNextTurnDraw g)
   | Just AHand = (g, acc ++ [RequestDrawHand (getTemporaryIdentifiers g PlayerA)])
   | Just BHand = (g, acc ++ [RequestDrawHand (getTemporaryIdentifiers g PlayerB)])
  | Just ASoul = (g, acc ++ [RequestDrawSoul (getTemporaryIdentifiers g PlayerA)])
   | Just BSoul = (g, acc ++ [RequestDrawSoul (getTemporaryIdentifiers g PlayerB)])
   | Nothing = (g, acc ++ [GameLogicError]) {-(g,acc) {-send message-}-}
  | (initiative,turnNumber,player_A,player_B,SpawnPhase,asp,bsp) = (g,acc) {-send message-}
  
  | (initiative,turnNumber,player_A,player_B,SpellPhase,asp,bsp) with (spawn player_A, spawn player_B)
   | _ = ?hole   
  
  
  
  
-}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  {-
   | (Just (MonsterCard cardA),Just (MonsterCard cardB)) with (spawnSkill cardA, spawnSkill cardB)
    | _ = ?hole


















    
    |(Just (skillA, usedA, costA),Just (skillB, usedB, costB)) = ?g
    |(Just (skillA, usedA, costA),Nothing) = ?g

    |(Nothing, Just (skillB, usedB, costB)) = ?g
    |(Nothing,Nothing) = stepGame (goToNextPhase (g,acc))
   | (Just (SpellCard cardA), Just (MonsterCard cardB)) with (spawnSkill cardA)
    | (skillA, usedA, costA) = ?g {-if usedA || costA > (fromIntegerNat (extractBounded (thoughts (player_A g)))) then let (g', cu) = discardUsedSpell g in stepGame(g', acc ++ cu) else  ?hole -}
   | (Just (MonsterCard cardA),Just (SpellCard cardB)) with (spawnSkill cardA, spawnSkill cardB)
    | (Just (skillA, usedA, costA), (skillB, usedB, costB)) = ?g
    | (Nothing, (skillB, usedB, costB)) = ?g
   | (Just (SpellCard cardA), Just (SpellCard cardB))   with (spawnSkill cardA, spawnSkill cardB)
    | ((skillA, usedA, costA), (skillB, usedB, costB)) = ?g
   | (Just (MonsterCard cardA), Nothing)  with (spawnSkill cardA)
    | Just (skillA, usedA, costA) = ?g
    | Nothing = ?g
   | (Just (SpellCard cardA), Nothing) = ?g
   | (Nothing, Just (MonsterCard cardB)) with (spawnSkill cardB)
    | Just (skillB, usedB, costB) = ?g {-let (g', acc') = killFatallyDamaged (g, acc) in stepGame (record {phase = RemovalPhase} g, acc' ++ [SpellPhaseToRemovalPhase]) -}
    | Nothing = ?g
   | (Nothing, Just (SpellCard cardB)) with (spawnSkill cardB)
    | (skillB, usedB, costB) = ?g
   | (Nothing, Nothing) = stepGame (goToNextPhase (g,acc))
  | (initiative,turnNumber,player_A_,player_B_,RemovalPhase,asp,bsp) with (deathQueue g)
   | [] = stepGame (goToNextPhase (g,acc))
   | (deadMonster :: deadMonsters) with (getMonsterField player_A_ player_B_ deadMonster)
    | Nothing= (g, acc ++ [GameLogicError])
    | Just (player,location,monster) with (aliveness (basic monster))
     | Alive = (g, acc ++ [GameLogicError])
     | DeadFresh= stepGame (record {deathQueue = deadMonsters} g, acc)
     | DeadStale with (level (basic monster))
      |(_,_,baseLevel)=
        if (temporaryId player_A_) == (temporaryId player)
           then let g' = (record {player_A->thoughts = transformBounded (\x => x + (extractBounded baseLevel)) (thoughts player_A_), player_A->graveyard = ((graveyard player_A_) ++ [MonsterCard (reviveCard monster)]),player_A->board = replaceAt (board player_A_) location Nothing,deathQueue = deadMonsters} g) in
                    let (a,b) = ((temporaryId player_A_), (temporaryId player_B_)) in
                        stepGame (damageSoul (g', acc ++ [(LoseSoulPoint a b), SendBoardToGraveyard location a b, (UpdateThoughts (thoughts (player_A g')) a b)]) (player_A g') (S Z))
           else let g' = (record {player_B->thoughts = transformBounded (\x => x + (extractBounded baseLevel)) (thoughts player_B_),player_B->graveyard = ((graveyard player_B_) ++ [MonsterCard (reviveCard monster)]),player_B->board = replaceAt (board player_B_) location Nothing,deathQueue = deadMonsters} g) in
                    let (a,b) = ((temporaryId player_A_), (temporaryId player_B_)) in
                        stepGame (damageSoul (g', acc ++ [(LoseSoulPoint b a), SendBoardToGraveyard location b a, (UpdateThoughts (thoughts (player_B g')) b a)]) (player_B g') (S Z))
  | (initiative,turnNumber,player_A,player_B,StartPhase,asp,bsp) = ?g
  | (initiative,turnNumber,player_A,player_B,EngagementPhase,asp,bsp) = ?g {-represent going to the next phase if no skills pending, etc, no units disengaged-}
  | (initiative,turnNumber,player_A,player_B,EndPhase,asp,bsp)  = ?g
  | (initiative,turnNumber,player_A,player_B,RevivalPhase,asp,bsp)  = ?g
  | (initiative,turnNumber,player_A,player_B,DeploymentPhase,asp,bsp) with (spawn player_A, spawn player_B)
   | (Nothing, Nothing) = stepGame (goToNextPhase (g,acc))
   | (Just (MonsterCard cardA), Nothing) = if boardFull (board player_A) then ?g else (g, acc ++ [DeployCardRequest (temporaryId player_A)])
   | (Nothing, Just(MonsterCard cardB)) = if boardFull (board player_B) then ?g else (g, acc ++ [DeployCardRequest (temporaryId player_B)])
   | (Just (MonsterCard cardA), Just (MonsterCard cardB)) = if boardFull (board player_A) then ?g else if boardFull (board player_B) then ?g
                                                            else (g, acc ++ [DeployCardRequest (temporaryId (getPlayer g initiative))])
   | (Just (SpellCard cardA),_) = (g, acc ++ [GameLogicError])
   | (_,Just (SpellCard cardB)) = (g, acc ++ [GameLogicError])


-}







