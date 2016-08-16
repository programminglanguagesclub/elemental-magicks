import Data.Vect


main : IO ()
main = return ()




{-Right now I'm completely ignoring next. Env for that is somewhat more complicated, since I need to remember previous environments...
I'm going to take out next for now. If I need it later I can deal with that...
-}


data SkillEffect = Dummy
{-data Binding = Bound Nat | Unbound String-}
data Condition = ConditionTrue {-other options including things which might be false depending on the selection-}


data Env = MkEnv (List (String,Nat))

{-While I say Game, I think I really just mean Player and Opponent. Then the skill data structures are actually held separately from this...-}
data Game = MkGame
data Message = MkMessage

mutual
  data Nonautomatic : Nat -> Type where
    TerminatedSkill : Nonautomatic 0
    Existential : (n : Nat) -> (Vect n String) -> Condition -> Automatic -> Automatic -> Nonautomatic n
  data Automatic = MkAutomatic (List SkillEffect) (Nonautomatic n)

data Skill = MkSkill (List SkillEffect) (Nonautomatic n)



{-NEED TO REVIEW TO MAKE SURE ENV IS USED CORRECTLY....-}

{- I'll probably need to pass in an environment into this one as well -}
satisfiableExistentialCondition : Vect n String -> Condition -> Bool
satisfiableExistentialCondition arguments condition = True

satisfiedExistentialCondition : Vect n String -> Vect n Nat -> Condition -> Bool

applySkillEffect : SkillEffect -> Game -> (Game,List Message)
applySkillEffect Dummy MkGame = (MkGame, [MkMessage])

applySkillEffects : List SkillEffect -> Game -> (Game,List Message)
applySkillEffects [] game = (game, [])
applySkillEffects (effect::effects) game =
  let (game',messages) = applySkillEffect effect game in
      let (game'',messages') = applySkillEffects effects game' in
          (game'',messages ++ messages')



step_interp : Automatic -> Game -> Env -> (Game, List Message, (n ** Nonautomatic n), Env)
step_interp (MkAutomatic skillEffects nonautomatic) game env =
  let (game', messages) = applySkillEffects skillEffects game in
      case nonautomatic of
           TerminatedSkill => (game',messages,(0 ** TerminatedSkill),env)
           Existential n arguments condition selected failed => case satisfiableExistentialCondition arguments condition of
                                                                     True => (game', messages, (n ** nonautomatic),env)
                                                                     False => let (game'', messages', nonautomatic',env') = step_interp selected game' env in
                                                                                  (game'', messages ++ messages', nonautomatic',env')


{-note that selection isn't the positions; it's the temporary ids of the cards selected-}
{-I can require the move to be satisfiable at the type level, but ignore that for now I guess?-}
move_interp : Nonautomatic n -> Vect n Nat -> Game -> Env -> (Game, List Message, (m ** Nonautomatic m),Env)
move_interp TerminatedSkill _ game env = (game,[],(0 ** TerminatedSkill),env) {-error case?-}
move_interp (Existential n arguments condition selected failed) selection game env with (satisfiedExistentialCondition arguments selection condition)
  | False = (game, [], (n ** (Existential n arguments condition selected failed)),env) {-could add a "failed selection" message-}
  | True = step_interp selected game env










