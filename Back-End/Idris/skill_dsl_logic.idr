module Skill_dsl_logic
import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import objects_basic
import skill_dsl_data
import skill_dsl
import phase
import clientupdates
import player
%access public export





{-Right now I'm completely ignoring next. Env for that is somewhat more complicated, since I need to remember previous environments...
I'm going to take out next for now. If I need it later I can deal with that...
-}


data Env = MkEnv (List (String,Nat))


{- I'll probably need to pass in an environment into this one as well -}
satisfiableExistentialCondition : Vect n String -> Condition -> Env -> Bool
satisfiableExistentialCondition arguments condition env = True

satisfiedExistentialCondition : Vect n String -> Vect n Nat -> Condition -> Env -> Bool {-this probably needs the players too as arguments...-}


applySkillEffect : SkillEffect -> Player -> Player -> Env -> (Player,Player,List ClientUpdate)
applySkillEffect skillEffect player opponent env = ?hole {-(player, opponent, [])-}




applySkillEffects : List SkillEffect -> Player -> Player -> Env -> (Player,Player,List ClientUpdate)
applySkillEffects [] player opponent env = (player, opponent, [])
applySkillEffects (effect::effects) player opponent env =
  let (player',opponent',updates) = applySkillEffect effect player opponent env in
      let (player'',opponent'',updates') = applySkillEffects effects player' opponent' env in
          (player'',opponent'',updates ++ updates')



step_interp : Automatic -> Player -> Player -> Env -> (Player,Player, List ClientUpdate, Nonautomatic, Env)
step_interp (MkAutomatic skillEffects nonautomatic) player opponent env =
  let (player',opponent', messages) = applySkillEffects skillEffects player opponent env in
      case nonautomatic of
           TerminatedSkill => (player',opponent',messages,TerminatedSkill,env)
           Existential arguments condition selected failed => case satisfiableExistentialCondition arguments condition env of
                                                                     True => (player',opponent', messages, nonautomatic, env)
                                                                     False => let (player'',opponent'', messages', nonautomatic',env') = step_interp selected player' opponent' env in
                                                                                  (player'',opponent'', messages ++ messages', nonautomatic',env')


extend_env : Env -> Vect n String -> Vect n Nat -> Env
{-nmm,... should really do this with n == m...-}
                           {-
extend_env (MkEnv env) arguments selection = MkEnv (env ++ (toList (zip arguments selection)))
-}

{-note that selection isn't the positions; it's the temporary ids of the cards selected-}
{-I can require the move to be satisfiable at the type level, but ignore that for now I guess?-}

alignVectors : Vect n a -> Vect m b -> Maybe (Vect n a, Vect n b)
alignVectors [] [] = Just ([],[])
alignVectors [] _  = Nothing
alignVectors _ []  = Nothing
alignVectors {n=S n'} {m=S m'} (x::xs) (y::ys) with (decEq n' m')
  | Yes prf    = Just (x::xs, rewrite prf in y::ys)
  | No  contra = Nothing

move_interp : Nonautomatic -> Vect n Nat -> Player -> Player -> Env -> (Player,Player, List ClientUpdate,Nonautomatic,Env)
move_interp TerminatedSkill _ player opponent env = (player,opponent,[],TerminatedSkill,env) {-error case?-}
move_interp (Existential arguments condition selected failed) selection player opponent env with (alignVectors arguments selection)
  | Nothing = (player,opponent,[],Existential arguments condition selected failed, env)
  | Just (arguments', selection')  = case satisfiedExistentialCondition arguments' selection' condition env of
                                          False => (player,opponent, [], Existential arguments condition selected failed,env) {-could add a "failed selection" message-}
                                          True => step_interp selected player opponent (extend_env env arguments' selection')



