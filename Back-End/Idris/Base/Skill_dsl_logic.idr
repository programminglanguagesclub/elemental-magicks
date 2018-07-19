module Base.Skill_dsl_logic

import Data.Vect
import Base.Preliminaries
import Base.Bounded
import Base.Hp
import Base.Clientupdates
import Base.Player
import Base.Card
import Base.Objects_basic
import Base.Skill_dsl_data
%access public export
%default total


{-Right now I'm completely ignoring next. Env for that is somewhat more complicated, since I need to remember previous environments...
I'm going to take out next for now. If I need it later I can deal with that...
-}


data Env = MkEnv (List (String,Nat))



{-right now there is no mechanism to force a selected card to be friendly or enemy !!!!!!!!!!!!!!!!!!!!!-}
{-also need to deal with forcing uniqueness of selection here and with the user input-}


{-
getValidTargets' : List (Maybe Monster) -> List Monster
getValidTargets' [] = []
getValidTargets' (Nothing::xs) = getValidTargets' xs
getValidTargets' ((Just monster)::xs) = monster :: (getValidTargets' xs)

getValidTargets : Player -> List Monster
getValidTargets player = ?hole {-getValidTargets' $ toList $ board player-}
-}

{-this probably should have a function for what is valid... -}

-------------------------------------------------------------------------------
lookupStat :
 BasicFieldedMonster ->
 StatR ->
 Integer

lookupStat b TemporaryAttackR = extractBounded $ getTemporary $ attack b
lookupStat b PermanentAttackR = extractBounded $ getPermanent $ attack b
lookupStat b TemporarySpeedR = extractBounded $ getTemporary $ speed b
lookupStat b PermanentSpeedR = extractBounded $ getPermanent $ speed b
lookupStat b TemporaryDefenseR = extractBounded $ getTemporary $ defense b
lookupStat b PermanentDefenseR = extractBounded $ getPermanent $ defense b
lookupStat b TemporaryRangeR = extractBounded $ getTemporary $ range b
lookupStat b PermanentRangeR = extractBounded $ getPermanent $ range b
lookupStat b TemporaryLevelR = extractBounded $ getTemporary $ level b
lookupStat b PermanentLevelR = extractBounded $ getPermanent $ level b
lookupStat b HpR = extractBounded $ getCurrentHp $ hp $ b
lookupStat b MaxHpR = extractBounded $ getMaxHp $ hp $ b
{-I also need to be able to access the base stats....-}
-------------------------------------------------------------------------------
correctId : Nat -> Maybe FieldedMonster -> Bool -- and one for unfielded?
correctId _ Nothing = False
correctId id' (Just monster) = (id (basic monster)) == id'
-------------------------------------------------------------------------------
lookupBasicCard : Nat -> Player -> Player -> Maybe BasicFieldedMonster {-no targetting spell cards for now!-}  -- and one for unfielded?
lookupBasicCard temporaryId player opponent = ?hole



{- case find (correctId temporaryId) (board player) of
                                                   Just (Just monster) => Just (basic monster)
                                                   Just _ => Nothing {-THIS CASE SHOULD NEVER HAPPEN...-}
                                                   Nothing => case find (correctId temporaryId) (board opponent) of
                                                                   Just (Just monster) => Just (basic monster)
                                                                   Just _ => Nothing {-This case should never happen-}
                                                                   Nothing => Nothing -}

-------------------------------------------------------------------------------
lookupCardId' : String -> List (String,Nat) -> Maybe Nat
lookupCardId' s [] = Nothing
lookupCardId' s ((s',n)::xs) with (s==s')
  | False = lookupCardId' s xs
  | True = Just n
lookupCardId : String -> Env -> Maybe Nat
lookupCardId s (MkEnv env) = lookupCardId' s env
-------------------------------------------------------------------------------
getValue :
 RInteger ->
 Player ->
 Player ->
 Env ->
 Maybe Integer

getValue (Constant x) _ _ _ = Just x

getValue (Variable statR var) player opponent env =
 lookupCardId var env >>= \id => {-THIS IS CURRENTLY ONLY SET UP TO LOOK FOR THINGS IN THE FRIENDLY AND ENEMY BOARDS!!!!-}
 lookupBasicCard id player opponent >>= \basicMonster =>
 pure $ lookupStat basicMonster statR

getValue (Plus a b) player opponent env =
 getValue a player opponent env >>= \x =>
 getValue b player opponent env >>= \y =>
 pure $ x+y

getValue (Minus a b) player opponent env =
 getValue a player opponent env >>= \x =>
 getValue b player opponent env >>= \y =>
 pure $ x-y

getValue (Mult a b) player opponent env =
 getValue a player opponent env >>= \x =>
 getValue b player opponent env >>= \y =>
 pure $ x*y

getValue (ThoughtsR b) player opponent env =
 Just $ extractBounded $ thoughtsResource $ if b then player else opponent

getValue (SchoolR b s) player opponent env =
 Just $ extractBounded $ index s $ knowledge $ if b then player else opponent

getValue (Cardinality var set condition) player opponent env = ?hole

getValue (Evoker statR) player opponent env = ?hole {- In addition the env, I should probably keep the id of the current evoker,
         which can be loaded when the skill is first put onto the head -}

-------------------------------------------------------------------------------
{-SOMEWHERE I HAVE OT MAKE SURE THAT WITH EACH SELECTION MADE THE CARDS ARE UNIQUE??!!-}
satisfiedExistentialCondition :
 Condition ->
 Player ->
 Player ->
 Env ->
 Maybe Bool

-- Is this only maybe because my selection can fail because
-- I don't have a proper card dictionary implemented yet?

satisfiedExistentialCondition Never _ _ _ = Just False
satisfiedExistentialCondition Vacuous _ _ _ = Just True
satisfiedExistentialCondition (RDead var) player opponent env =
 lookupCardId var env >>= \id =>
 lookupBasicCard id player opponent >>= \card =>
 case aliveness card of
  Alive => Just False
  DeadFresh => Just True
  DeadStale => Just True -- should be made into a function delegation and not boolean blind case statement.

-- ought to be able to have less repeated code for these conditions....
satisfiedExistentialCondition (NotX evokerId arg) player opponent env = ?hole

satisfiedExistentialCondition (LT a b) player opponent env =
 getValue a player opponent env >>= \x =>
 getValue b player opponent env >>= \y =>
 pure $ x < y 

satisfiedExistentialCondition (EQ a b) player opponent env =
 getValue a player opponent env >>= \x =>
 getValue b player opponent env >>= \y =>
 pure $ x == y

satisfiedExistentialCondition (GT a b) player opponent env =
 getValue a player opponent env >>= \x =>
 getValue b player opponent env >>= \y =>
 pure $ x > y

satisfiedExistentialCondition (LEQ a b) player opponent env =
 getValue a player opponent env >>= \x =>
 getValue b player opponent env >>= \y =>
 pure $ x <= y

satisfiedExistentialCondition (GEQ a b) player opponent env =
 getValue a player opponent env >>= \x =>
 getValue b player opponent env >>= \y =>
 pure $ x >= y

satisfiedExistentialCondition (And cond1 cond2) player opponent env =
 satisfiedExistentialCondition cond1 player opponent env >>= \x =>
 satisfiedExistentialCondition cond2 player opponent env >>= \y =>
 pure $ x && y


satisfiedExistentialCondition (Or cond1 cond2) player opponent env =
 satisfiedExistentialCondition cond1 player opponent env >>= \x =>
 satisfiedExistentialCondition cond2 player opponent env >>= \y =>
 pure $ x || y

satisfiedExistentialCondition (Not cond) player opponent env =
 satisfiedExistentialCondition cond player opponent env >>= \x =>
 pure $ not x

{- these need a relative set and side as well -}
satisfiedExistentialCondition (Exists var cond) player opponent env = ?hole
satisfiedExistentialCondition (All var cond) player opponent env = ?hole
-------------------------------------------------------------------------------
satisfiedExistentialCondition' :
 Condition ->
 Player ->
 Player ->
 Env ->
 Bool {-until I add more error handling or more type stuff, for now just treat nothing as false-}

satisfiedExistentialCondition' condition player opponent env =
 case satisfiedExistentialCondition condition player opponent env of
  Nothing => False
  Just b => b
-------------------------------------------------------------------------------
extend_env :
 Env ->
 Vect n String ->
 Vect n Nat ->
 Env

extend_env (MkEnv env) arguments selection =
 MkEnv(env ++ (toList $ zip arguments selection))
-------------------------------------------------------------------------------
satisfiableExistentialCondition' :
 List String ->
 List FieldedMonster ->
 List FieldedMonster ->
 Condition ->
 Player ->
 Player ->
 Env ->
 Bool

satisfiableExistentialCondition' [] _ _ condition player opponent env with (satisfiedExistentialCondition condition player opponent env)
  | Just True = True
  | _ = False
satisfiableExistentialCondition' (arg::args) _ [] _ _ _ _ = False
satisfiableExistentialCondition' (arg::args) later (target::targets) condition player opponent env =
 case satisfiableExistentialCondition' args [] (later ++ targets) condition player opponent (extend_env env [arg] [id $ basic target]) of
  True => True
  False => satisfiableExistentialCondition' (arg::args) (target::later) targets condition player opponent env 


--Eventually this might be done by trying assignments which are locations.
--For now we're going to get a list of monsters and use those as the valid assignments
--The assignment is assumed that it has to be unique (I STILL HAVE TO SET THIS UP FOR THE PLAYER'S CHOICE TOO>>>>>).
-------------------------------------------------------------------------------
satisfiableExistentialCondition :
 Vect n String ->
 Condition ->
 Player ->
 Player ->
 Env ->
 Bool {-for now, don't try to optimize this: just try all assignments-}


{-
satisfiableExistentialCondition arguments condition player opponent env =
  satisfiableExistentialCondition'
   (toList arguments)
   []
   ((getValidTargets player) ++ (getValidTargets opponent))
   condition
   player
   opponent
   env
   -}
-------------------------------------------------------------------------------
updateMonster :
 BasicFieldedMonster ->
 Player ->
 Player ->
 (Player, Player) {-updates the monster where it belongs?-}

updateMonster basicMonster player opponent = ?hole
-------------------------------------------------------------------------------
applySkillEffect :
 SkillEffect ->
 Player ->
 Player ->
 Env ->
 (Player,Player,List ClientUpdate)

applySkillEffect skillEffect player opponent env = ?hole {-(player, opponent, [])-}
-------------------------------------------------------------------------------
applySkillEffects :
 List SkillEffect ->
 Player ->
 Player ->
 Env ->
 (Player,Player,List ClientUpdate)

applySkillEffects [] player opponent env = (player, opponent, [])

applySkillEffects (effect::effects) player opponent env =
 let (player',opponent',updates) = applySkillEffect effect player opponent env in
 let (player'',opponent'',updates') = applySkillEffects effects player' opponent' env in
 (player'',opponent'',updates ++ updates')
-------------------------------------------------------------------------------
getValidBindings :
 String ->
 Condition ->
 Player ->
 Player ->
 Env ->
 List Nat

getValidBindings argument condition player opponent env = ?hole
-------------------------------------------------------------------------------
step_interp :
 Automatic ->
 Player ->
 Player ->
 Env ->
 (Player,Player, List ClientUpdate, Nonautomatic, Env)

step_interp (MkAutomatic skillEffects nonautomatic cardId playerId) player opponent env =
 let (player',opponent', messages) = applySkillEffects skillEffects player opponent env in
 case nonautomatic of
  TerminatedSkill =>
   (player',opponent',messages,TerminatedSkill,env)
  Existential arguments condition selected failed cardId' playerId' =>
   let (variables,sets) = unzip arguments in
   case satisfiableExistentialCondition variables condition player opponent env of
    True => (player',opponent', messages, nonautomatic, env)
    False =>
     let (player'',opponent'', messages', nonautomatic',env') = step_interp (assert_smaller (MkAutomatic skillEffects nonautomatic cardId playerId) failed) player' opponent' env in
     (player'',opponent'', messages ++ messages', nonautomatic',env')

step_interp (Universal argument condition skillEffects next cardId playerId) player opponent env = ?hole
-------------------------------------------------------------------------------

{-note that selection isn't the positions; it's the temporary ids of the cards selected-}
{-I can require the move to be satisfiable at the type level, but ignore that for now I guess?-}

alignVectors : Vect n a -> Vect m b -> Maybe (Vect n a, Vect n b)
alignVectors [] [] = Just ([],[])
alignVectors [] _  = Nothing
alignVectors _ []  = Nothing
alignVectors {n=S n'} {m=S m'} (x::xs) (y::ys) with (decEq n' m')
  | Yes prf    = Just (x::xs, rewrite prf in y::ys)
  | No  contra = Nothing

-------------------------------------------------------------------------------
move_interp :
 (selection : Vect n (String,Set)) -> 
 (condition : Condition) -> 
 (ifSelected : Automatic) ->
 (ifUnable : Automatic) ->
 (cardId : Nat) -> 
 (playerId : String) ->
 (friendlyFieldSelection : List Nat) ->
 (enemyFieldSelection : List Nat) ->
 (friendlyHandSelection : List Nat) ->
 (enemyHandSelection : List Nat) ->
 (friendlyGraveyardSelection : List Nat) ->
 (enemyGraveyardSelection : List Nat) ->
 (friendlyBanishedSelection : List Nat) ->
 (enemyBanishedSelection : List Nat) ->
 List Nat ->
 Player ->
 Player ->
 Env -> -- if game over, who won                                    which is which?
 (Either (Either Player Player) (Nonautomatic, List Skill, List Nat, Player, Player), List ClientUpdate)

 -----(Player,Player, List ClientUpdate,Nonautomatic,Env)

{-
move_interp skill friendlyFieldSelection enemyFieldSelection friendlyHandSelection enemyHandSelection friendlyGraveyardSelection enemyGraveyardSelection friendlyBanishedSelection enemyBanishedSelection player opponent env with (skill)
 | TerminatedSkill cardId playerId = 
    (player,opponent,[],TerminatedSkill cardId playerId,env) {-error case?-}
 | Existential args condition selected failed cardId playerId
   with (alignVectors args selection)
  | Nothing = (player,opponent,[],skill, env)
  | Just (args', selection') =
     let (variables',sets') = unzip args' in
     let env' = extend_env env variables' selection' in
     case satisfiedExistentialCondition' condition player opponent env' of
      False =>
       let ex = Existential args condition selected failed cardId playerId in
       (player,opponent,[],ex,env) 
--could add a "failed selection" message
      True => step_interp selected player opponent env'

-}

{-Somewhere I also want to take into account that certain skills can't be executed from certain areas: if a card has a skill queued but the card is moved to the graveyard, that probably ends the effect-}
{-Also need to keep track of the evoker of skills. Note I need to both know that certain skills can't be used from certain areas AND certain skills can't be used ON cards in certain areas...-}
-------------------------------------------------------------------------------
