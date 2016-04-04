module Main

import Data.Vect
import Data.Fin
import Data.So
import preliminaries
import phase
import objects
import skill_dsl
import serverupdates

{- these need to be made into datatypes because they need to store Env.-}
mutual

{- Need to concat in the env here!!!!-}
{-

Ah, so actually statement true is currently
AutomaticSkillComponent (m) (n) (p) env,
and it gets mapped to
AutomaticSkillComponent (m+m') (n+n') (p+p') env'

only when we are actually able to update env (produce env' that is),
which happens in the small-step interpreter (when we get the bindings from Condition, in other words)
-}

 NonautomaticSkillComponent : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type
 {-NonautomaticSkillComponent m n p env = {m' : Nat} -> {n' : Nat} -> {p' : Nat} -> ((Condition m n p m' n' p'), AutomaticSkillComponent m n p env, AutomaticSkillComponent m n p env, AutomaticSkillComponent m n p env) -}
 NonautomaticSkillComponent m n p env = {m' : Nat} -> {n' : Nat} -> {p' : Nat} -> ((Condition m' n' p'), AutomaticSkillComponent m n p env, AutomaticSkillComponent m n p env, AutomaticSkillComponent m n p env)

{- in particular n could be different for this last one-}

{- this is if(condition){do if true}{do if false}{do after if statement}-}
{- not n in all cases here. Again, just trying to get to typecheck for now. -}
{- Also there need to be two natural indices: one for selection of cards, and the other for selection of squares! -}
{- Oh, then there's everything regarding the set location. That's a bit tricky because you shouldn't be able to affect its stats, so it's like a Card, but it can be empty, which is like a Square...-}
{- Actually, the set location doesn't require being selected, although it does get bound (you can do things to the card that was on the set position at a particular time) -}
{- maybe it doesn't get bound -}

{- These skill effects are bounded by m n and p, but I want it to type check if it has less restricted bounds... -}

 AutomaticSkillComponent : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type
 AutomaticSkillComponent m n p env = (List (SkillEffect m n p env), Maybe (NonautomaticSkillComponent m n p env))

Skill : Type
Skill = AutomaticSkillComponent 0 0 0 empty_env

{- still have to represent rounds, initiative -}

breakUp : Skill -> AutomaticSkillComponent 0 0 0 empty_env
breakUp s = s

{-
breakUp : AutomaticSkillComponent m n p env -> (List (SkillEffect), Maybe (NonautomaticSkillComponent m n p env))
breakUp x = x
-}

record Game where
 constructor MkGame
 round : Bounded 0 1
 player_A_Initiative : Bool
 turnNumber : Nat
 m : Nat
 n : Nat
 p : Nat

{- skillHead index. I might want skillHead to not be a maybe, and consider "Nothing" to be where n = 0. Not sure. -}
 
 env : Env m n p
 skillHead : Maybe (NonautomaticSkillComponent m n p env)
 skillQueue : List (AutomaticSkillComponent 0 0 0 empty_env) {-Skill-}
 player_A : Player 0 0
 player_B : Player 0 0
 phase : Phase

syntax "new" "game" [tokenA] [tokenB] = MkGame (0 ** Oh) True 0 0 0 0 (Vect.Nil,Vect.Nil,Vect.Nil) Nothing [] (new player tokenA) (new player tokenB) DrawPhase
{- game : Game
game = new game "playerAToken" "playerBToken" -}
{-For now, I am ignoring spells.-}
data ClientUpdate = ClientDummy

ServerUpdateWrapper : Type
ServerUpdateWrapper = (ServerUpdate, String)

{-need to create update stuff, but for now ignore-}

{- For now I'm ignoring the distinction between client update and client update wrappers that define who the update gets sent to -}

Selection : {b : Nat} -> {h : Nat} -> {g : Nat} -> (game : Game) -> (Vect b BoardIndex, Vect h HandIndex, Vect g GraveyardIndex) -> (Game, List ClientUpdate)
Selection game (board, hand, graveyard) with (skillHead game)
 | Nothing = (game, [])
 | skill = (game, [])


getSoulPoints : Player h g -> Nat
getSoulPoints player = 0 {-dummy-}

FooDrawCard : Player n m -> Card -> Player (S n) m
FooDrawCard player card = MkPlayer (board player) (reverse (card :: (reverse (hand player)))) (graveyard player) (spawn player) (soul player) (thoughts player) (knowledge player) (token player)


{-the following currently ignores client updates-}

executeSkillEffects : Game -> List (SkillEffect m n p env) -> Game
executeSkillEffects g a = g {-dummy-}

stepGame : Game -> Game
{-
This will perform some automatically executing change. E.g. change in phase, etc.
Each self-recursive call of stepGame corresponds to a single small step in the game update
(perhaps going to the next phase, executing an automatic skill component, resolving a user skill selection, etc.)
-}

{-eventually encode in the type of game a constraint that it cannot reach 0 soul points for both players. This probably means not just doing getSoulPoints since that's a function.... -}
stepGame g with (m g, n g, p g, env g, skillHead g, skillQueue g)
 | (m, n, p, env, Just skillHead, skillQueue) = g {-should first check to see if a skill selection can be made, because we might have to keep going...-}
 | (m, n, p, env, Nothing, ((skillEffects, next)::skillQueue)) = stepGame (executeSkillEffects (record {skillHead = next} g) skillEffects)
 | (m, n, p, env, Nothing, []) with (round g, player_A_Initiative g, turnNumber g, player_A g, player_B g, phase g, getSoulPoints (player_A g), getSoulPoints (player_B g))
  | (round,player_A_Initiative,turnNumber,player_A,player_B,phase,Z,bsp) = g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,phase,asp,Z) = g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,phase,asp,bsp) = g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,DrawPhase,asp,bsp) with (spawn player_A, spawn player_B)
   | (Just cardA,Just cardB) = g {- record { phase = nextPhase DrawPhase } g -}
   | _ = g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,SpawnPhase,asp,bsp) = g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,SpellPhase,asp,bsp) = g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,RemovalPhase,asp,bsp) = g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,StartPhase,asp,bsp) = g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,EngagementPhase,asp,bsp) = g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,EndPhase,asp,bsp) = g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,RevivalPhase,asp,bsp) = g
  | (round,player_A_Initiative,turnNumber,player_A,player_B,DeploymentPhase,asp,bsp) = g
  | _ = g

{-Need to cause units to leave the field if not revived in order of death, and then in order of position on the field. For this we need another data structure in game to represent the order of death-}

{-For now, completely ignore the possibility of the user using skills! :D -}

transformGame : Game -> ServerUpdate -> (Game, List ClientUpdate)
transformGame game serverupdate with (phase game,serverupdate)
 | (DrawPhase,DrawCard id) = (game, []) {-Maybe-}
 | (DrawPhase,_) = (game,[]) {-No-}
 | (SpawnPhase,SetCard schools cardIndex) = (game, [])
 | (SpawnPhase,Skip schools) = (game, [])
 | (SpawnPhase, _) = (game,[])
 | (SpellPhase,SkillSelection n) = (game,[]) {-again, this (the n) is currently indexed incorrectly-}
 | (SpellPhase,_) = (game,[])
 | (RemovalPhase,SkillSelection n) = (game,[]) {-again, this (the n) is currently indexed incorrectly-}
 | (RemovalPhase,_) = (game,[])
 | (StartPhase,SkillSelection n) = (game,[]) {-again, this (the n) is currently indexed incorrectly-}
 | (StartPhase,_) = (game,[])
 | (EngagementPhase, AttackRow n) = (game,[])
 | (EngagementPhase, Rest) = (game,[])
 | (EngagementPhase, DirectAttack) = (game,[])
 | (EngagementPhase, Move) = (game,[])
 | (EngagementPhase, SkillInitiation n) = (game,[])
 | (EngagementPhase, SkillSelection n) = (game,[]) {-again, this (the n) is currently indexed incorrectly-}
 | (EngagementPhase,_) = (game,[])
 | (EndPhase,SkillSelection n) = (game,[]) {-again, this (the n) is currently indexed incorrectly-}
 | (EndPhase,_) = (game,[])
 | (RevivalPhase,Revive b) = (game, [])
 | (RevivalPhase,_) = (game, [])


while_loop : List Game -> ServerUpdate -> (List Game, List ClientUpdate)
while_loop [] _ = ([],[])
while_loop (g::gs) _ = ([],[])

main : IO ()
main = putStrLn "hello world"











