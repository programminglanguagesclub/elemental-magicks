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

applySkillEffect : SkillEffect -> Game -> Env -> (Game,List Message)
applySkillEffect Dummy MkGame env = (MkGame, [])
applySkillEffect (IncrementTemporaryAttack var) MkGame env = (MkGame, [MkMessage])

applySkillEffects : List SkillEffect -> Game -> Env -> (Game,List Message)
applySkillEffects [] game env = (game, [])
applySkillEffects (effect::effects) game env =
  let (game',messages) = applySkillEffect effect game env in
      let (game'',messages') = applySkillEffects effects game' env in
          (game'',messages ++ messages')



step_interp : Automatic -> Game -> Env -> (Game, List Message, (n ** Nonautomatic n), Env)
step_interp (MkAutomatic skillEffects nonautomatic) game env =
  let (game', messages) = applySkillEffects skillEffects game env in
      case nonautomatic of
           TerminatedSkill => (game',messages,(0 ** TerminatedSkill),env)
           Existential n arguments condition selected failed => case satisfiableExistentialCondition arguments condition env of
                                                                     True => (game', messages, (n ** nonautomatic),env)
                                                                     False => let (game'', messages', nonautomatic',env') = step_interp selected game' env in
                                                                                  (game'', messages ++ messages', nonautomatic',env')


extend_env : Env -> Vect n String -> Vect n Nat -> Env
extend_env (MkEnv env) arguments selection = MkEnv (env ++ (toList (zip arguments selection)))


{-note that selection isn't the positions; it's the temporary ids of the cards selected-}
{-I can require the move to be satisfiable at the type level, but ignore that for now I guess?-}
move_interp : Nonautomatic n -> Vect n Nat -> Game -> Env -> (Game, List Message, (m ** Nonautomatic m),Env)
move_interp TerminatedSkill _ game env = (game,[],(0 ** TerminatedSkill),env) {-error case?-}
move_interp (Existential n arguments condition selected failed) selection game env with (satisfiedExistentialCondition arguments selection condition env)
  | False = (game, [], (n ** (Existential n arguments condition selected failed)),env) {-could add a "failed selection" message-}
  | True = step_interp selected game (extend_env env arguments selection)








