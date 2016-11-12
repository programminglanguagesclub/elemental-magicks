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


{-This can cause skills to be pushed onto the skill queues, so this needs to be declared mutually with
stepGame.

in stepGameNoSkills, after the delegated call... if there is anything on the queues, stepGame needs to be called.
otherwise, we're done.

-}


{-on one of these we need to know the turn number potentially? (need to damage soul at some point) -}
stepGameNoSkills : (WhichPlayer, Nat, Nonautomatic, List Automatic, List Nat, Player, Player, Phase, List ClientUpdate) -> (Game, List ClientUpdate) {- assumes that skillHead g and skillQueue g are empty -}
stepGameNoSkills (initiative, turnNumber, skillHead, skillQueue, deathQueue, player_A, player_B, phase,acc) with phase
  | DrawPhase = stepDrawPhase initiative player_A player_B
  | SpawnPhase = stepSpawnPhase initiative {-?(turnNumber g)-} deathQueue player_A player_B
  | SpellPhase = stepSpellPhase initiative {-?(turnNumber g)-} deathQueue player_A player_B
  | RemovalPhase = stepRemovalPhase deathQueue player_A player_B
  | StartPhase = stepStartPhase initiative deathQueue player_A player_B
  | EngagementPhase = stepEngagementPhase initiative deathQueue player_A player_B
  | EndPhase = stepEndPhase initiative deathQueue player_A player_B
  | RevivalPhase = stepRevivalPhase player_A player_B
  | DeploymentPhase = stepDeploymentPhase player_A player_B

{- on some of these, I might not have to do anything at all... -}

{-

record Game where
 constructor MkGame
 initiative : WhichPlayer
 turnNumber : Nat
 skillHead : Nonautomatic 
 skillQueue : List Automatic
 deathQueue : List Nat {-The temporary ids of the monster (maybe this should have its own type?)-}
 player_A : Player
 player_B : Player
 phase : Phase


-}



stepGame : (Game,List ClientUpdate) -> (Game,List ClientUpdate)
stepGame (g,acc) with (skillHead g, skillQueue g)
  | (TerminatedSkillComponent, []) = stepGameNoSkills (g,acc)
  | (TerminatedSkillComponent, (pendingSkill::pendingSkills)) = ?hole {-stepGame (record {skillHead = pendingSkill, skillQueue = pendingSkills} g,acc) -}{-wrong type... need to execute head first... -}
  | _ = ?hole 

