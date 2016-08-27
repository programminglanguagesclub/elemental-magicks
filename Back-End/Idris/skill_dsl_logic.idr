module Skill_dsl_logic
import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import hp
import objects_basic
import skill_dsl_data
import skill_dsl
import phase
import clientupdates
import player
%access public export
%default total




{-Right now I'm completely ignoring next. Env for that is somewhat more complicated, since I need to remember previous environments...
I'm going to take out next for now. If I need it later I can deal with that...
-}


data Env = MkEnv (List (String,Nat))



{-right now there is no mechanism to force a selected card to be friendly or enemy !!!!!!!!!!!!!!!!!!!!!-}
{-also need to deal with forcing uniqueness of selection here and with the user input-}



getValidTargets' : List (Maybe Monster) -> List Monster
getValidTargets' [] = []
getValidTargets' (Nothing::xs) = getValidTargets' xs
getValidTargets' ((Just monster)::xs) = monster :: (getValidTargets' xs)

getValidTargets : Player -> List Monster
getValidTargets player = getValidTargets' $ toList $ board player







   



lookupStat : BasicMonster -> StatR -> Integer 
lookupStat basicMonster TemporaryAttackR = extractBounded $ getTemporary $ attack basicMonster
lookupStat basicMonster PermanentAttackR = extractBounded $ getPermanent $ attack basicMonster
lookupStat basicMonster TemporarySpeedR = extractBounded $ getTemporary $ speed basicMonster
lookupStat basicMonster PermanentSpeedR = extractBounded $ getPermanent $ speed basicMonster
lookupStat basicMonster HpR = extractBounded $ getCurrentHp $ hp $ basicMonster
lookupStat basicMonster MaxHpR = extractBounded $ getMaxHp $ hp $ basicMonster


correctId : Nat -> Maybe Monster -> Bool
correctId _ Nothing = False
correctId id (Just monster) = (temporaryId (basic monster)) == id

lookupBasicCard : Nat -> Player -> Player -> Maybe BasicMonster {-no targetting spell cards for now!-} 
lookupBasicCard temporaryId player opponent = case find (correctId temporaryId) (board player) of
                                                   Just (Just monster) => Just (basic monster)
                                                   Just _ => Nothing {-THIS CASE SHOULD NEVER HAPPEN...-}
                                                   Nothing => case find (correctId temporaryId) (board opponent) of
                                                                   Just (Just monster) => Just (basic monster)
                                                                   Just _ => Nothing {-This case should never happen-}
                                                                   Nothing => Nothing
lookupCardId' : String -> List (String,Nat) -> Maybe Nat
lookupCardId' s [] = Nothing
lookupCardId' s ((s',n)::xs) with (s==s')
  | False = lookupCardId' s xs
  | True = Just n
lookupCardId : String -> Env -> Maybe Nat
lookupCardId s (MkEnv env) = lookupCardId' s env


getValue : RInteger -> Player -> Player -> Env -> Maybe Integer
getValue (Constant x) _ _ _ = Just x
getValue (Variable statR var) player opponent env = do id <- lookupCardId var env
                                                       basicMonster <- lookupBasicCard id player opponent
                                                       return (lookupStat basicMonster statR)



getValue (Plus a b) player opponent env = do x <- getValue a player opponent env
                                             y <- getValue b player opponent env
                                             return (x+y)
getValue (Minus a b) player opponent env = do x <- getValue a player opponent env
                                              y <- getValue b player opponent env
                                              return (x-y)
getValue (ThoughtsR b) player opponent env = Just (extractBounded $ thoughts (if b then player else opponent))
getValue (SchoolR b s) player opponent env = Just (extractBounded $ index s (knowledge (if b then player else opponent)))
getValue (Cardinality var set condition) player opponent env = ?hole

{-SOMEWHERE I HAVE OT MAKE SURE THAT WITH EACH SELECTION MADE THE CARDS ARE UNIQUE??!!-}
satisfiedExistentialCondition : Condition -> Player -> Player -> Env -> Maybe Bool
satisfiedExistentialCondition Vacuous _ _ _ = Just True
satisfiedExistentialCondition (RDead var) player opponent env = do id <- lookupCardId var env
                                                                   card <- lookupBasicCard id player opponent
                                                                   return (case aliveness card of
                                                                                Alive => True
                                                                                DeadFresh => False
                                                                                DeadStale => True)
satisfiedExistentialCondition (LT a b) player opponent env = do x <- getValue a player opponent env
                                                                y <- getValue b player opponent env
                                                                return (x < y) 
satisfiedExistentialCondition (EQ a b) player opponent env = do x <- getValue a player opponent env
                                                                y <- getValue b player opponent env
                                                                return (x == y)
satisfiedExistentialCondition (GT a b) player opponent env = do x <- getValue a player opponent env
                                                                y <- getValue b player opponent env
                                                                return (x > y)
satisfiedExistentialCondition (LEQ a b) player opponent env = do x <- getValue a player opponent env
                                                                 y <- getValue b player opponent env
                                                                 return (x <= y)
satisfiedExistentialCondition (GEQ a b) player opponent env = do x <- getValue a player opponent env
                                                                 y <- getValue b player opponent env
                                                                 return (x >= y)
satisfiedExistentialCondition (And cond1 cond2) player opponent env = do x <- satisfiedExistentialCondition cond1 player opponent env
                                                                         y <- satisfiedExistentialCondition cond2 player opponent env
                                                                         return (x && y)
satisfiedExistentialCondition (Or cond1 cond2) player opponent env = do x <- satisfiedExistentialCondition cond1 player opponent env
                                                                        y <- satisfiedExistentialCondition cond2 player opponent env
                                                                        return (x || y)

satisfiedExistentialCondition' : Condition -> Player -> Player -> Env -> Bool {-until I add more error handling or more type stuff, for now just treat nothing as false-}
satisfiedExistentialCondition' condition player opponent env = case satisfiedExistentialCondition condition player opponent env of
                                                                    Nothing => False
                                                                    Just b => b



extend_env : Env -> Vect n String -> Vect n Nat -> Env
extend_env (MkEnv env) arguments selection = MkEnv(env ++ (toList $ zip arguments selection))

satisfiableExistentialCondition' : List String -> List Monster -> List Monster -> Condition -> Player -> Player -> Env -> Bool
satisfiableExistentialCondition' [] _ _ condition player opponent env with (satisfiedExistentialCondition condition player opponent env)
  | Just True = True
  | _ = False
satisfiableExistentialCondition' (arg::args) _ [] _ _ _ _ = False
satisfiableExistentialCondition' (arg::args) later (target::targets) condition player opponent env =
  case satisfiableExistentialCondition' args [] (later ++ targets) condition player opponent (extend_env env [arg] [temporaryId $ basic target]) of
       True => True
       False => satisfiableExistentialCondition' (arg::args) (target::later) targets condition player opponent env 




{-eventually this might be done by trying assignments which are locations. For now we're going to get a list of monsters and use those as the valid assignments-}
{-the assignment is assumed that it has to be unique (I STILL HAVE TO SET THIS UP FOR THE PLAYER'S CHOICE TOO>>>>>).-}
satisfiableExistentialCondition : Vect n String -> Condition -> Player -> Player -> Env -> Bool {-for now, don't try to optimize this: just try all assignments-}
satisfiableExistentialCondition arguments condition player opponent env =
  satisfiableExistentialCondition' (toList arguments) [] ((getValidTargets player) ++ (getValidTargets opponent)) condition player opponent env




updateMonster : BasicMonster -> Player -> Player -> (Player, Player) {-updates the monster where it belongs?-}
updateMonster basicMonster player opponent = ?hole



applySkillEffect : SkillEffect -> Player -> Player -> Env -> (Player,Player,List ClientUpdate)
applySkillEffect skillEffect player opponent env = ?hole {-(player, opponent, [])-}

applySkillEffects : List SkillEffect -> Player -> Player -> Env -> (Player,Player,List ClientUpdate)
applySkillEffects [] player opponent env = (player, opponent, [])
applySkillEffects (effect::effects) player opponent env =
  let (player',opponent',updates) = applySkillEffect effect player opponent env in
      let (player'',opponent'',updates') = applySkillEffects effects player' opponent' env in
          (player'',opponent'',updates ++ updates')


getValidBindings : String -> Condition -> Player -> Player -> Env -> List Nat
getValidBindings argument condition player opponent env = ?hole

step_interp : Automatic -> Player -> Player -> Env -> (Player,Player, List ClientUpdate, Nonautomatic, Env)
step_interp (MkAutomatic skillEffects nonautomatic) player opponent env =
  let (player',opponent', messages) = applySkillEffects skillEffects player opponent env in
      case nonautomatic of
           TerminatedSkill => (player',opponent',messages,TerminatedSkill,env)
           Existential arguments condition selected failed => case satisfiableExistentialCondition arguments condition player opponent env of
                                                                   True => (player',opponent', messages, nonautomatic, env)
                                                                   False => let (player'',opponent'', messages', nonautomatic',env') =
                                                                                step_interp (assert_smaller (MkAutomatic skillEffects nonautomatic) failed) player' opponent' env in
                                                                                (player'',opponent'', messages ++ messages', nonautomatic',env')
step_interp (Universal argument condition skillEffects next) player opponent env = ?hole





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
  | Just (arguments', selection')  = case satisfiedExistentialCondition' condition player opponent (extend_env env arguments' selection') of
                                          False => (player,opponent, [], Existential arguments condition selected failed,env) {-could add a "failed selection" message-}
                                          True => step_interp selected player opponent (extend_env env arguments' selection')


{-Somewhere I also want to take into account that certain skills can't be executed from certain areas: if a card has a skill queued but the card is moved to the graveyard, that probably ends the effect-}
