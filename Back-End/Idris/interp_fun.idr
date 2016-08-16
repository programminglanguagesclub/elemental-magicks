import Data.Vect


main : IO ()
main = return ()

data SkillEffect = Dummy
data Binding = Bound Nat | Unbound String
data Condition = ConditionTrue {-other options including things which might be false depending on the selection-}

{-While I say Game, I think I really just mean Player and Opponent. Then the skill data structures are actually held separately from this...-}
data Game = MkGame
data Message = MkMessage

mutual
  data Nonautomatic : Nat -> Type where
    TerminatedSkill : Nonautomatic 0
    Existential : (n : Nat) -> (Vect n String) -> Condition -> Automatic -> Automatic -> Automatic -> Nonautomatic n
  data Automatic = MkAutomatic (List SkillEffect) (Nonautomatic n)

data Skill = MkSkill (List SkillEffect) (Nonautomatic n)


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



step_interp : Automatic -> Game -> (Game, List Message, (n ** Nonautomatic n))
step_interp (MkAutomatic skillEffects nonautomatic) game =
  let (game', messages) = applySkillEffects skillEffects game in
      case nonautomatic of
           TerminatedSkill => (game',messages,(0 ** TerminatedSkill))
           Existential n arguments condition selected failed next => case satisfiableExistentialCondition arguments condition of
                                                                          True => (game', messages, (n ** nonautomatic))
                                                                          False => let (game'', messages', nonautomatic') = step_interp selected game' in
                                                                                       (game'', messages ++ messages', nonautomatic')


{-note that selection isn't the positions; it's the temporary ids of the cards selected-}
{-I can require the move to be satisfiable at the type level, but ignore that for now I guess?-}
move_interp : Nonautomatic n -> Vect n Nat -> Game -> (Game, List Message, (m ** Nonautomatic m))
move_interp Terminated _ _ = ?hole {-error case?-}
move_interp (Existential n arguments condition selected failed next) selection game with (satisfiedExistentialCondition arguments selection condition)
  | False = (game, [], (n ** (Existential n arguments condition selected failed next))) {-could add a "failed selection" message-}
  | True = step_interp selected game



{-
interp : Nonautomatic n -> Vect n Nat -> Game -> (Game,List Message,(m**Nonautomatic m))
interp TerminatedSkill [] g = (g,[],(0**TerminatedSkill))
{-interp (Existential n arguments condition selected failed next) selection game = (game, MkMessage, (n ** (Existential n arguments condition selected failed next)))-}
interp (Existential n arguments condition selected failed next) selection game with (satisfiableExistentialCondition arguments selection condition)
  | True = (game, [], (n ** (Existential n arguments condition selected failed next)))
  | False = case failed of
                 TerminatedSkill => (game, [], (0 ** TerminatedSkill))
                 Existential n' arguments' condition' selected' failed' next' => (n' ** failed)



-}










