module Main.Step_game
import Data.Fin
import Base.Phase
import Base.Skill_dsl_data
import Base.Player
import Main.Game
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





{- need to modify signature to not take a game but take the components other than the skills, as those are assumed
 to be empty or whatever.
-}


{-This can cause skills to be pushed onto the skill queues, so this needs to be declared mutually with
stepGame.

in stepGameNoSkills, after the delegated call... if there is anything on the queues, stepGame needs to be called.
otherwise, we're done.

-}







{- Because stepping the game can cause a skill to trigger, I *might* need to have skills being loaded
   to the skill head / queue to trigger client updates....
-}




{- Eventually return two Strings too, which is the next instruction for the players. For now, don't give instructions -}



{-



INITIATIVE CAN BE CALCULATED FROM TURNNUMBER, AND SHOULD NOT BE AN ADDITIONAL FIELD OF GAME

-}




{-
getMessage : (WhichPlayer, List Nat, Player, Player, Phase, Nonautomatic, List Automatic) -> ClientInstruction
getMessage (initiative, deathQueue, playerA, playerB, phase, skillHead, skillQueue) with (phase)
  | DrawPhase = getMessageDrawPhase playerA playerB
  | SpawnPhase = getMessageSpawnPhase initiative playerA playerB
  | SpellPhase = getMessageSpellPhase playerA playerB skillHead skillQueue
  | RemovalPhase = getMessageRemovalPhase playerA playerB skillHead skillQueue
  | StartPhase = getMessageStartPhase initiative playerA playerB skillHead skillQueue
  | EngagementPhase = getMessageEngagementPhase initiative playerA playerB skillHead skillQueue
  | EndPhase = getMessageEndPhase initiative playerA playerB skillHead skillQueue
  | RevivalPhase = getMessageRevivalPhase initiative playerA playerB
  | DeploymentPhase = getMessageDeploymentPhase initiative playerA playerB
-}


{-STEP GAME IS ASSERTED TO BE TOTAL WITHOUT PROOF. PROVING STEP GAME IS TOTAL MAY BE A SOMEWHAT MAJOR PROJECT -}

{-Note that I have to be careful that I am not adding update messages which tell the user what to do, and cause an infinite recursion here.-}
{-also mutual with stepGameNoSkills and stepGame -}
mutual {- drag along a boolean argument which says if we're done stepping -}
  continueStep :
   (Game, List ClientUpdate, Maybe ClientInstruction) ->
   (Game, List ClientUpdate, ClientInstruction)
  stepGameNoSkills :
   (WhichPlayer, Nat, List Nat, Phase, List ClientUpdate) ->
   (Game, List ClientUpdate, ClientInstruction)
  stepGame :
   (Game, List ClientUpdate) ->
   (Game, List ClientUpdate, ClientInstruction)
  continueStep (game,updates,Just clientInstruction) =
   (game,updates,clientInstruction)
  continueStep (game,updates,Nothing) =
   stepGame (game,updates)
  {-on one of these we need to know the turn number potentially? (need to damage soul at some point) -}
 
  stepGameNoSkills (initiative, turnNumber, deathQueue, phase, acc) with (phase)
    | DrawPhase playerA playerB cardsDrawn =
      case stepDrawPhase playerA playerB of
       Nothing =>
        let (game',acc') = goToNextPhase (MkGame initiative 0 TerminatedSkill [] [] (DrawPhase playerA playerB cardsDrawn), acc) in
        continueStep (game', acc', Nothing)
       Just clientInstruction =>
        continueStep (MkGame initiative 0 TerminatedSkill [] [] (DrawPhase playerA playerB cardsDrawn), acc, Just clientInstruction)
    | MkPhaseCycle SpawnPhase playerA playerB =
      case stepSpawnPhase initiative playerA playerB of
       Nothing =>
        let (game', acc') = goToNextPhase (MkGame initiative turnNumber TerminatedSkill [] deathQueue (MkPhaseCycle SpawnPhase playerA playerB), acc) in   
        continueStep (game', acc', Nothing)
       Just clientInstruction =>
        continueStep (MkGame initiative turnNumber TerminatedSkill [] deathQueue (MkPhaseCycle SpawnPhase playerA playerB), acc, Just clientInstruction)
 
    | MkPhaseCycle SpellPhase playerA playerB = continueStep (stepSpellPhase initiative turnNumber deathQueue playerA playerB)
    
    | MkPhaseCycle RemovalPhase playerA playerB = continueStep (stepRemovalPhase deathQueue playerA playerB)
    
    | MkPhaseCycle StartPhase playerA playerB = continueStep (stepStartPhase initiative deathQueue playerA playerB)
    
    | MkPhaseCycle EngagementPhase playerA playerB = continueStep (stepEngagementPhase initiative deathQueue playerA playerB)
    
    | MkPhaseCycle EndPhase playerA playerB = continueStep (stepEndPhase initiative deathQueue playerA playerB)
    
    | MkPhaseCycle RevivalPhase playerA playerB = continueStep (stepRevivalPhase playerA playerB)
    
    | MkPhaseCycle DeploymentPhase playerA playerB = continueStep (stepDeploymentPhase playerA playerB)
     
  stepGame  = ?hole {-(g,acc) with (skillHead g, skillQueue g)
    | (TerminatedSkillComponent, []) = assert_total $ stepGameNoSkills (initiative g, turnNumber g, deathQueue g, player_A g, player_B g, phase g, acc)
    | (TerminatedSkillComponent, (pendingSkill::pendingSkills)) = assert_total ?hole {-stepGame (record {skillHead = pendingSkill, skillQueue = pendingSkills} g,acc) -}{-wrong type... need to execute head first... -}
    | _ = ?hole
    -}
