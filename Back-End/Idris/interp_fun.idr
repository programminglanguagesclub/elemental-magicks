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





applySkillEffect : SkillEffect -> Game -> (Game,List Message)
applySkillEffect Dummy MkGame = (MkGame, [MkMessage])

applySkillEffects : List SkillEffect -> Game -> (Game,List Message)
applySkillEffects [] game = (game, [])
applySkillEffects (effect::effects) game =
  let (game',messages) = applySkillEffect effect game in
      let (game'',messages') = applySkillEffects effects game' in
          (game'',messages ++ messages')

interp : Nonautomatic n -> Vect n Nat -> Game -> (Game,Message,(m**Nonautomatic m))
interp TerminatedSkill [] g = (g,MkMessage,(0**TerminatedSkill))
interp (Existential n arguments condition selected failed next) selection game = (game, MkMessage, (n ** (Existential n arguments condition selected failed next)))














