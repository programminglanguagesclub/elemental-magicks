module Main.Step_game
import Base.Phase
import Base.Skill_dsl_data
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
stepGameNoSkills : (Game, List ClientUpdate) -> (Game, List ClientUpdate) {- assumes that skillHead g and skillQueue g are empty -}
stepGameNoSkills (g,acc) with (phase g)
  | DrawPhase = stepDrawPhase (initiative g) (player_A g) (player_B g)
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

