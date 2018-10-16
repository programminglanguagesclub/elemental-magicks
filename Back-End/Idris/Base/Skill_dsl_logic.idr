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


data Env = MkEnv (List (String,(Fin 25, WhichPlayer)))



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
{-


 data FiveOptions a b c d e
    = BoardLocation a
       | HandLocation b
          | GraveyardLocation c
             | BanishedLocation d
                | SpawnLocation e
                 
                  getCard :
                    {player : Player} ->
                      (correctPlayer : CorrectPlayer player) ->
                        Fin 25 ->
                          FiveOptions

                          -}



correctId : Fin 25 -> FieldedMonster -> Bool -- and one for unfielded?
correctId id' monster = (id (basic monster)) == id'
-------------------------------------------------------------------------------
lookupBasicCard : (Fin 25, WhichPlayer) -> Player -> Player -> Maybe BasicFieldedMonster {-no targetting spell cards for now!-}  -- and one for unfielded?
lookupBasicCard temporaryId player opponent = ?hole {-case Data.Vect.find (\maybeMonster => Just temporaryId == (id . basic) <$> maybeMonster) (flattenBoard $ board player) of
                                                _ => ?hole
                                                 {-  Just (Just monster) => Just (basic monster)
                                                   Just _ => Nothing {-THIS CASE SHOULD NEVER HAPPEN...-}
                                                   Nothing => case find (correctId temporaryId) (flattenBoard $ board opponent) of
                                                                   Just (Just monster) => Just (basic monster)
                                                                   Just _ => Nothing {-This case should never happen-}
                                                                   Nothing => Nothing 
                                                                   -}-}
                                                                   

--lookupBasicCard : Fin 25 -> Player -> Player -> 
-------------------------------------------------------------------------------
lookupCardId' : String -> List (String,(Fin 25, WhichPlayer)) -> Maybe (Fin 25, WhichPlayer)
lookupCardId' s [] = Nothing
lookupCardId' s ((s',n)::xs) with (s==s')
  | False = lookupCardId' s xs
  | True = Just n
lookupCardId : String -> Env -> Maybe (Fin 25, WhichPlayer)
lookupCardId s (MkEnv env) = lookupCardId' s env
-------------------------------------------------------------------------------
getSide : Side -> Env -> (evokerPlayer : WhichPlayer) -> WhichPlayer
getSide Friendly _ evokerPlayer = evokerPlayer
getSide Enemy _ evokerPlayer = getOpponent evokerPlayer
getSide (FriendlyVar var) env _ = ?hole
getSide (EnemyVar var) env _ = ?hole
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
 Vect n (Fin 25, WhichPlayer) ->
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
satisfiableExistentialCondition' (arg::args) later (target::targets) condition player opponent env = ?hole
{-
 case satisfiableExistentialCondition' args [] (later ++ targets) condition player opponent (extend_env env [arg] ([id $ basic target], ?whichPlayer)) of
  True => True
  False => satisfiableExistentialCondition' (arg::args) (target::later) targets condition player opponent env 
  -}

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
removeEvokerSkillsFromQueue :
 (Fin 25, WhichPlayer) ->
 (skillQueue : List (Skill, Fin 25, WhichPlayer, SkillType)) ->
 List (Skill, Fin 25, WhichPlayer, SkillType)

removeEvokerSkillsFromQueue _ [] = []
removeEvokerSkillsFromQueue (cardId, evokerPlayer) ((skill,queuedId,queuedPlayer,skillType)::skillTail) with (cardId == queuedId, evokerPlayer == queuedPlayer)
  | (True, True) = removeEvokerSkillsFromQueue (cardId, evokerPlayer) skillTail
  | _ = ((skill,queuedId,queuedPlayer,skillType)::(removeEvokerSkillsFromQueue (cardId, evokerPlayer) skillTail))


{-

   = EvokerSkillEffectStatEffect StatEffect 
      | SkillEffectStatEffect StatEffect String 
         | SkillEffectResourceEffect ResourceEffect 
            | SkillEffectPositionEffect PositionEffect 
               | SkillEffectConditional Condition SkillEffect SkillEffect 
                  | SkillEffectRowEffect Side String SkillEffect String
                       --does effect to all units in row of unit bound to string;
                            --the last string binds the respective units in the row for use in SkillEffect
                               | SkillEffectColumnEffect Side String SkillEffect String
                                    --does effect to all units in column of unit bound to string;
                                         --the last string binds the respective units in the column for use in SkillEffect
                                            | SkillEffectBehind Side String SkillEffect String
                                               | SkillEffectInFront Side String SkillEffect String
                                                  | SkillEffectRightOf Side String SkillEffect String
                                                     | SkillEffectLeftOf Side String SkillEffect String
                                                        | SkillEffectBoardPositions Side (List (Bounded 1 9)) SkillEffect String --?
                                                           | SendSquareToGraveyard Side (Fin 9)
                                                              | SendSelfToGraveyard
                                                                 | SendVarToGraveyard -- okay, I think I will make var only be the field... not spawn.
                                                                    | SendSpawnToGraveyard Side
                                                                    -}

{-


   = ThoughtEffect Mutator RInteger {- Do I have side information for these? -} 
                                             | SchoolEffect (Fin 6) Mutator RInteger
                                                | DecrementLP RInteger


 getSide : Side -> Context -> (evokerPlayer : WhichPlayer) -> WhichPlayer

                                                -}




-- skillHead : Maybe (Nonautomatic, Fin 25, WhichPlayer, Env) -- evokerId, whichPlayer
applySkillEffect :
 SkillEffect ->
 (player : Player) ->
 (opponent : Player) ->
 (context : Env) ->
 (evokerId : (Fin 25, WhichPlayer)) ->
 (deathQueue : List (Fin 25, WhichPlayer)) ->
 (env : Env) ->
 (next : Nonautomatic) ->
 (skillQueue : List (Skill, Fin 25, WhichPlayer, SkillType)) -> -- skill, evokerId, whichPlayer, skillType
 (Player,Player,List (Fin 25, WhichPlayer), List (Skill, Fin 25, WhichPlayer, SkillType), List ClientUpdate)

--applySkillEffect skillEffect player opponent deathQueue env = ?hole {-(player, opponent, [])-}
applySkillEffect (EvokerSkillEffectStatEffect statEffect) player opponent context evokerId deathQueue env next skillQueue = -- what do I do about "next" here. I am not manipulating the head...
 ?hole
applySkillEffect (SkillEffectStatEffect statEffect string) player opponent context evokerId deathQueue env next skillQueue = ?hole
applySkillEffect (SkillEffectResourceEffect side (ThoughtEffect mutator rInteger)) player opponent context (evokerId, evokerPlayer) deathQueue env next skillQueue =
 let p = getSide side context evokerPlayer in ?hole
applySkillEffect (SkillEffectResourceEffect side (SchoolEffect whichSchool mutator rInteger)) player opponent context (evokerId, evokerPlayer) deathQueue env next skillQueue =
 let p = getSide side context evokerPlayer in
 let val = getValue rInteger player opponent context in -- val is a maybe right now.... :/
 case val of
  Nothing => ?hole
  Just value => asgfsagjdasjdgajklgs
    

applySkillEffect (SkillEffectResourceEffect side (DecrementLP rInteger)) player opponent context (evokerId, evokerPlayer) deathQueue env next skillQueue =
 let p = getSide side context evokerPlayer in ?hole

applySkillEffect (SkillEffectPositionEffect positionEffect) player opponent context evokerId deathQueue env next skillQueue = ?hole
applySkillEffect (SkillEffectConditional condition skillEffectTrue skillEffectFalse) player opponent context evokerId deathQueue env next skillQueue = ?hole
applySkillEffect (SkillEffectRowEffect side string1 skillEffect string2) player opponent context evokerId deathQueue env next skillQueue = ?hole
applySkillEffect (SkillEffectColumnEffect side string1 skillEffect string2) player opponent context evokerId deathQueue env next skillQueue = ?hole
applySkillEffect (SkillEffectBehind side string1 skillEffect string2) player opponent context evokerId deathQueue env next skillQueue = ?hole
applySkillEffect (SkillEffectInFront side string1 skillEffect string2) player opponent context evokerId deathQueue env next skillQueue = ?hole
applySkillEffect (SkillEffectRightOf side string1 skillEffect string2) player opponent context evokerId deathQueue env next skillQueue = ?hole
applySkillEffect (SkillEffectLeftOf side string1 skillEffect string2) player opponent context evokerId deathQueue env next skillQueue = ?hole
applySkillEffect (SkillEffectBoardPositions side positions skillEffect string) player opponent context evokerId deathQueue env next skillQueue = ?hole
applySkillEffect (SendSquareToGraveyard side position) player opponent context evokerId deathQueue env next skillQueue = ?hole
applySkillEffect SendSelfToGraveyard player opponent context evokerId deathQueue env next skillQueue =
  -- need to remove every pending skill which has this card as the evoker.
  -- also actually move card to graveyard.
  let skillQueue' = removeEvokerSkillsFromQueue evokerId skillQueue in ?hole
  --let player' = ?hole in ?hole

applySkillEffect (SendVarToGraveyard string) player opponent context evokerId deathQueue env next skillQueue = ?hole
applySkillEffect (SendSpawnToGraveyard side) player opponent context evokerId deathQueue env next skillQueue = ?hole

-- More effects will be added.


{-

 removeEvokerSkillsFromQueue :
       dhdshdsfh (skillQueue : List (Skill, Fin 25, WhichPlayer, SkillType)) ->
         List (Skill, Fin 25, WhichPlayer, SkillType)
         -}

-------------------------------------------------------------------------------
applySkillEffects :
 List SkillEffect ->
 (player : Player) ->
 (opponent : Player) ->
 (evokerId : (Fin 25, WhichPlayer)) ->
 List (Fin 25, WhichPlayer) ->
 Env ->
 (Player,Player,List (Fin 25, WhichPlayer), List ClientUpdate)

applySkillEffects [] player opponent evokerId deathQueue env = ?hole --(player, opponent, deathQueue, [])

applySkillEffects (effect::effects) player opponent evokerId deathQueue env = ?hole
{-
 let (player',opponent',deathQueue', updates) = applySkillEffect effect player opponent evokerId deathQueue env in
 let (player'',opponent'',deathQueue'',updates') = applySkillEffects effects player' opponent' evokerId deathQueue' env in
     (player'',opponent'',deathQueue'', updates ++ updates') -}
-------------------------------------------------------------------------------
{-getValidBindings :
 String ->
 Condition ->
 Player ->
 Player ->
 Env ->
 List Nat

getValidBindings argument condition player opponent env = ?hole
-}
-------------------------------------------------------------------------------
step_interp :
 Automatic ->
 (evokerId : (Fin 25, WhichPlayer)) ->
 (player : Player) ->
 (opponent : Player) ->
 (skillType : SkillType) ->
 List (Fin 25, WhichPlayer) ->
 Env ->
 (Player,Player, List (Fin 25, WhichPlayer), List ClientUpdate, Maybe Nonautomatic)

step_interp (MkAutomatic skillEffects nonautomatic) evokerId player opponent skillType deathQueue env = -- should care about skill type...
 let (player',opponent',deathQueue', messages) = applySkillEffects skillEffects player opponent evokerId deathQueue env in
 case nonautomatic of
  Nothing =>
   (player',opponent',deathQueue',messages,Nothing)
  Just (Existential arguments condition selected failed) =>
   let (variables,sets) = unzip arguments in
   case satisfiableExistentialCondition variables condition player opponent env of
    True => (player',opponent', deathQueue', messages, nonautomatic)
    False =>
     let
      (player'',opponent'', deathQueue', messages', nonautomatic') =
     
      step_interp
       (assert_smaller (MkAutomatic skillEffects nonautomatic) failed)
       evokerId
       player'
       opponent'
       skillType
       deathQueue'
       env
     in
     (player'',opponent'', deathQueue', messages ++ messages', nonautomatic')

step_interp (Universal argument condition skillEffects next) evokerId player opponent skillType deathQueue env = ?hole
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
 (arguments : Vect n (String,Set)) ->
 (condition : Condition) -> 
 (ifSelected : Automatic) ->
 (ifUnable : Automatic) ->
 (cardId : (Fin 25, WhichPlayer)) -> 
 (friendlyFieldSelection : List (Fin 9)) ->
 (enemyFieldSelection : List (Fin 9)) ->
 (friendlyHandSelection : List Nat) ->
 (enemyHandSelection : List Nat) ->
 (friendlyGraveyardSelection : List Nat) ->
 (enemyGraveyardSelection : List Nat) ->
 (friendlyBanishedSelection : List Nat) ->
 (enemyBanishedSelection : List Nat) ->
 (deathQueue : List (Fin 25, WhichPlayer)) ->
 Player ->
 Player ->
 Env -> -- if game over, who won                                                                         which is which?
 (Either WhichPlayer (Maybe Nonautomatic, List (Skill, Fin 25, WhichPlayer, SkillType), List (Fin 25, WhichPlayer), Player, Player), List ClientUpdate)


{-

data Set
   = FriendlyBoard 
    | EnemyBoard 
     | FriendlySpawn 
      | EnemySpawn 
       | FriendlyHand 
        | EnemyHand 
         | FriendlyGraveyard 
          | EnemyGraveyard 
           | FriendlyBanished
            | EnemyBanished
             | Union Set Set

             -}

move_interp
 arguments
 condition
 ifSelected
 ifUnable
 cardId
 friendlyFieldSelection
 enemyFieldSelection
 friendlyHandSelection
 enemyHandSelection
 friendlyGraveyardSelection
 enemyGraveyardSelection
 friendlyBanishedSelection
 enemyBanishedSelection
 deathQueue
 player
 opponent
 env
 = ?hole
 --with (alignVectors selection 
 {-
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
