module Main.Step_game
import Data.Fin
import Data.Vect
import Base.Preliminaries
import Base.Phase
import Base.Skill_dsl_data
import Base.Card
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
-- I can just load both spawn skills at the start of the spawn phase. No bug.




I can achieve this by pushing both of the spawn skills onto the queue/head/whatever at the start of the spell phase (during the transition to the spell phase)

-}

{- Because stepping the game can cause a skill to trigger, I *might* need to have skills being loaded
   to the skill head / queue to trigger client updates....
-}




{- Eventually return two Strings too, which is the next instruction for the players. For now, don't give instructions -}

--INITIATIVE CAN BE CALCULATED FROM TURNNUMBER, AND SHOULD NOT BE AN ADDITIONAL FIELD OF GAME


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
-------------------------------------------------------------------------------
  continueStep (game,updates,Just clientInstruction) =
   (game,updates,clientInstruction)
  continueStep (game,updates,Nothing) =
   stepGame (game,updates)
   {-on one of these we need to know the turn number potentially? (need to damage soul at some point) -}
-------------------------------------------------------------------------------
  stepGameNoSkills initiative turnNumber deathQueue phase playerA playerB acc with (phase)
   | SpawnPhase =
       case stepSpawnPhase initiative playerA playerB of
        Nothing =>
         let playerOnMove' = initiative in
         let (game', acc') = goToNextPhase (MkGame turnNumber Nothing [] deathQueue SpawnPhase playerA playerB playerOnMove', acc) in   
         continueStep (game', acc', Nothing)
        Just clientInstruction =>
         let playerOnMove' = initiative in
         (MkGame turnNumber Nothing [] deathQueue SpawnPhase playerA playerB playerOnMove', acc, clientInstruction)
   
   | SpellPhase = continueStep (stepSpellPhase initiative turnNumber deathQueue playerA playerB)
    
   | RemovalPhase = continueStep (stepRemovalPhase deathQueue playerA playerB)
    
   | StartPhase = continueStep (stepStartPhase initiative deathQueue playerA playerB)
    
   | EngagementPhase = continueStep (stepEngagementPhase initiative deathQueue playerA playerB)
    
   | EndPhase = continueStep (stepEndPhase initiative deathQueue playerA playerB)
    
   | RevivalPhase = ?hole --continueStep (stepRevivalPhase playerA playerB)
    
   | DeploymentPhase = continueStep (stepDeploymentPhase playerA playerB)
-------------------------------------------------------------------------------     
  stepGame (g,acc) with (skillHead g, skillQueue g)
   | (Nothing, []) =
    assert_total $
    stepGameNoSkills (getInitiative g) (turnNumber g) (deathQueue g) (phase g) (playerA g) (playerB g) acc
   
   | (Nothing, (pendingSkill::pendingSkills)) =
    assert_total ?hole

{-stepGame (record {skillHead = pendingSkill, skillQueue = pendingSkills} g,acc) -}{-wrong type... need to execute head first... -}
   
   | (Just (Existential arguments condition successBranch failureBranch, cardId, whichPlayer), skillQueue) = ?hole
-------------------------------------------------------------------------------    


