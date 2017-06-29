module Main.Step_game
import Data.Fin
import Data.Vect
import Base.Preliminaries
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

-------------------------------------------------------------------------------
mutual
-------------------------------------------------------------------------------
  continueStep :
   (Game, List ClientUpdate, Maybe ClientInstruction) ->
   (Game, List ClientUpdate, ClientInstruction)
-------------------------------------------------------------------------------
  stepGameNoSkills :
   WhichPlayer ->
   Nat ->
   List Nat ->
   Phase ->
   Player ->
   Player ->
   List ClientUpdate ->
   (Game, List ClientUpdate, ClientInstruction)
-------------------------------------------------------------------------------  
  stepGame :
   (Game, List ClientUpdate) ->
   (Game, List ClientUpdate, ClientInstruction)
-------------------------------------------------------------------------------

-- SOMEWHERE I NEED TO INCREASE THE TURN NUMBER!!



-- I should have a generic continue stepping thing,
-- which is responsible for killing dead units, etc.

-- I actually might be able to get away with not having these specialized stepping functions and just doing that...
-- at least to an extent (I can't quite load all skills onto the queue at the start of the end and start phases tho, sadly,
-- otherwise units killed by one skill will not die until the rest of the skills go off...)

  continueStep (game,updates,Just clientInstruction) =
   (game,updates,clientInstruction)
  continueStep (game,updates,Nothing) =
   stepGame (game,updates)
   {-on one of these we need to know the turn number potentially? (need to damage soul at some point) -}
 
  stepGameNoSkills initiative turnNumber deathQueue phase playerA playerB acc with (phase)
 {-| DrawPhase playerA playerB cardsDrawn =
       case stepDrawPhase playerA playerB of
        Nothing =>
         let (game',acc') = goToNextPhase (MkGame initiative 0 TerminatedSkill [] [] (DrawPhase playerA playerB cardsDrawn), acc) in
         continueStep (game', acc', Nothing)
        Just clientInstruction =>
         continueStep (MkGame initiative 0 TerminatedSkill [] [] (DrawPhase playerA playerB cardsDrawn), acc, Just clientInstruction)-}
     
   | SpawnPhase =
       case stepSpawnPhase initiative playerA playerB of
        Nothing =>
         let (game', acc') = goToNextPhase (MkGame turnNumber TerminatedSkill [] deathQueue SpawnPhase playerA playerB, acc) in   
         continueStep (game', acc', Nothing)
        Just clientInstruction =>
         (MkGame turnNumber TerminatedSkill [] deathQueue SpawnPhase playerA playerB, acc, clientInstruction)
  
   | SpellPhase = continueStep (stepSpellPhase initiative turnNumber deathQueue playerA playerB)
    
   | RemovalPhase = continueStep (stepRemovalPhase deathQueue playerA playerB)
    
   | StartPhase = continueStep (stepStartPhase initiative deathQueue playerA playerB)
    
   | EngagementPhase = continueStep (stepEngagementPhase initiative deathQueue playerA playerB)
    
   | EndPhase = continueStep (stepEndPhase initiative deathQueue playerA playerB)
    
   | RevivalPhase = continueStep (stepRevivalPhase playerA playerB)
    
   | DeploymentPhase = continueStep (stepDeploymentPhase playerA playerB)
     
  stepGame (g,acc) with (skillHead g, skillQueue g)
   | (TerminatedSkill, []) = assert_total $ stepGameNoSkills (getInitiative g) (turnNumber g) (deathQueue g) (phase g) (playerA g) (playerB g) acc
   | (TerminatedSkill, (pendingSkill::pendingSkills)) = assert_total ?hole
{-stepGame (record {skillHead = pendingSkill, skillQueue = pendingSkills} g,acc) -}{-wrong type... need to execute head first... -}
   | (Existential arguments condition successBranch failureBranch cardId playerId, skillQueue) = ?hole
    


-- I need to have additional checks when skills are activated. Thus, death skills can load at any time, but only activate when they meet their cost and condition, etc.
-- This requires some special cases for different types of skills, perhaps added automatically to their data structures ( so everything is very homogeneous)
