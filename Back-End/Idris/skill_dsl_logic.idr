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






{-Right now I'm completely ignoring next. Env for that is somewhat more complicated, since I need to remember previous environments...
I'm going to take out next for now. If I need it later I can deal with that...
-}


data Env = MkEnv (List (String,Nat))


{- I'll probably need to pass in an environment into this one as well -}
satisfiableExistentialCondition : Vect n String -> Condition -> Env -> Bool
satisfiableExistentialCondition arguments condition env = True

satisfiedExistentialCondition : Vect n String -> Vect n Nat -> Condition -> Env -> Bool

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
           TerminatedSkill => (player',opponent',messages,(0 ** TerminatedSkill),env)
           Existential arguments condition selected failed => case satisfiableExistentialCondition arguments condition env of
                                                                     True => (player',opponent', messages, nonautomatic, env)
                                                                     False => let (player'',opponent'', messages', nonautomatic',env') = step_interp selected player' opponent' env in
                                                                                  (player'',opponent'', messages ++ messages', nonautomatic',env')


extend_env : Env -> Vect n String -> Vect n Nat -> Env
extend_env (MkEnv env) arguments selection = MkEnv (env ++ (toList (zip arguments selection)))


{-note that selection isn't the positions; it's the temporary ids of the cards selected-}
{-I can require the move to be satisfiable at the type level, but ignore that for now I guess?-}
move_interp : Nonautomatic n -> Vect n Nat -> Game -> Env -> (Game, List Message, (m ** Nonautomatic m),Env)
move_interp TerminatedSkill _ game env = (game,[],(0 ** TerminatedSkill),env) {-error case?-}
move_interp (Existential n arguments condition selected failed) selection game env with (satisfiedExistentialCondition arguments selection condition env)
  | False = (game, [], (n ** (Existential n arguments condition selected failed)),env) {-could add a "failed selection" message-}
  | True = step_interp selected game (extend_env env arguments selection)








