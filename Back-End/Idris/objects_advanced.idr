module Objects_advanced

import Data.Vect
import Data.So
import preliminaries
import objects_basic
import skill_dsl
import phase
import clientupdates

public export
Skill : Type
Skill = (SkillComponent, Bool, Nat) {- does a skill exist, and has it been used? This way I can also easily make it so that spell cards don't disappear as soon as they are used and monsters don't become engaged as they use their action skills-}

{- used : Bool, cost : Nat -}

public export
record Monster where
 constructor MkMonster
 basic        : BasicMonster
 autoSkill    : Maybe Skill
 startSkill   : Maybe Skill
 endSkill     : Maybe Skill
 usedEnd      : Maybe Skill
 spawnSkill   : Maybe Skill
 actionSkills : List Skill

public export
record Spell where
 constructor MkSpell
 basic      : BasicSpell
 spawnSkill : Skill

public export data Card = SpellCard Spell | MonsterCard Monster

{-
syntax monster [basic] [auto] [start] [end] [spawn] [actions] = MkMonster basic auto False start False end False spawn False actions False
syntax spell [basic] [spawn] = MkSpell basic spawn
-}
{-
mutant_pig : BasicMonster
mutant_pig = basic monster 20 0 2 1 3
-}





public export
Board : Type
Board = Vect 9 (Maybe Monster)
public export
Spawn : Type
Spawn = Maybe Card
public export
Soul : Type
Soul = Vect 5 (Maybe Monster) {- again more information could go in the type -}
public export
Thoughts : Type
Thoughts = Bounded 0 absoluteUpperBound
public export
Knowledge : Type
Knowledge = Vect 6 (Level)



{-

I'm making the LP on cards be either 1 or 2 (to start with. Then they can be 0, 1 or 2).
That way it decreases the max game length, and also adds more strategy (can't just mindlessly fill out a bunch of 3 LP cards)

-}




{-this might go in preliminaries-}
public export
transformThoughts : (Integer -> Integer) -> Thoughts -> Thoughts
transformThoughts = transformBounded 0 absoluteUpperBound Oh Oh

public export
record Player where
 constructor MkPlayer
 board           : Board {-Vect 9 (Maybe Monster)-}
 rowTarget       : Vect 3 (Fin 3)
 hand            : List Card
 graveyard       : List Card
 discard         : List Card
 spawn           : Spawn
 soul            : Soul
 thoughts        : Thoughts
 knowledge       : Knowledge
 temporaryId     : String






















public export
getLiving : Maybe Monster -> Bool
getLiving Nothing = False
getLiving (Just m) with (aliveness (basic m))
 | Alive = True
 | DeadFresh = False
 | DeadStale = False








{-getRowTarget : Player -> Fin 3 -> Maybe Nat {-This takes a player and a row, and returns the index of the next target, or nothing if there is no valid target (living monster)-}-}



public export
getRowTarget : Fin 3 -> Vect 9 (Maybe Monster) -> Fin 9 -> Fin 9 -> Fin 9 -> Maybe (Fin 9)
getRowTarget FZ m a b c with ((getLiving (index a m)),(getLiving (index b m)),(getLiving (index c m)))
 | (True,_,_) = Just a
 | (False,True,_) = Just b
 | (False,False,True) = Just c
 | (False,False,False) = Nothing
getRowTarget (FS FZ) m a b c with ((getLiving (index a m)),(getLiving (index b m)),(getLiving (index c m)))
 | (_,True,_) = Just b
 | (_,False,True) = Just c
 | (True,False,False) = Just a
 | (False,False,False) = Nothing
getRowTarget (FS (FS FZ)) m a b c with ((getLiving (index a m)),(getLiving (index b m)),(getLiving (index c m)))
 | (_,_,True) = Just c
 | (True,_,False) = Just a
 | (False,True,False) = Just b
 | (False,False,False) = Nothing


getNextRowTarget : Fin 3 -> Fin 3
getNextRowTarget FZ = FS FZ
getNextRowTarget (FS FZ) = FS (FS FZ)
getNextRowTarget (FS (FS FZ)) = FZ





{-
This doesn't quite work. I need to move to the next position AFTER the next card....
-}

{-

goToNextRowTarget : Player -> Fin 3 -> Player
goToNextRowTarget player n = case n of
                              FZ => record {rowTarget = replaceAt FZ (transformRowTarget) (rowTarget player))(rowTarget player)} player
                              FS FZ => record {rowTarget = (rowTarget player)} player
                              FS (FS FZ) => record {rowTarget = (rowTarget player)} player

-}


{-
goToNextRowTarget player n = case n of with (take 3 (drop (3)(board player)))
 | _ = player
-}




syntax "new" "player" [token] = MkPlayer (Vect.replicate 9 Nothing) [] [] [] Nothing (Vect.replicate 5 Nothing) (0 ** Oh) (Vect.replicate 6 (0 ** Oh)) token










public export
data WhichPlayer = PlayerA | PlayerB
public export
data Round = FirstRound | SecondRound


{-Reset used_death_skill, used_counter_skill before auto skill and action of card. -}


public export
record Game where
 constructor MkGame
 initiative : WhichPlayer
 turnNumber : Nat
 skillHead  : Maybe (Condition, SkillComponent, SkillComponent, SkillComponent)
 skillQueue : List SkillComponent
 deathQueue : List Nat {-The temporary ids of the monster (maybe this should have its own type?)-}
 player_A   : Player
 player_B   : Player
 phase      : Phase
 
syntax "new" "game" [tokenA] [tokenB] = MkGame PlayerA 0 (Vect.Nil,Vect.Nil,Vect.Nil) Nothing [] [] (new player tokenA) (new player tokenB) DrawPhase








public export
evaluateLazyInt : LazyInt -> (player : Player) -> (opponent : Player) -> Maybe Integer
evaluateLazyInt (LazyIntStat lazyIntStatType env statRValue nat) _ _ with (index' nat env)
 |Nothing = Nothing
 |Just basicMonster = Just (getStat lazyIntStatType statRValue basicMonster)
evaluateLazyInt (Constant integer) _ _ = Just integer
evaluateLazyInt (ThoughtsR CardOwner) player _ = Just (extractBounded (thoughts player))
evaluateLazyInt (ThoughtsR CardOpponent) _ opponent = Just (extractBounded (thoughts opponent))
evaluateLazyInt (SchoolR CardOwner school) player _ = Just (extractBounded (index school (knowledge player)))
evaluateLazyInt (SchoolR CardOpponent school) _ opponent = Just (extractBounded (index school (knowledge opponent)))


{- {-from skill_dsl:-}

public export
data SkillEffect = AttackL  Env Mutator StatLValue BoardMonsterVar LazyInt
                 | DefenseL Env Mutator StatLValue BoardMonsterVar LazyInt
                 | RangeL   Env Mutator StatLValue BoardMonsterVar LazyInt
                 | LevelL   Env Mutator StatLValue BoardMonsterVar LazyInt
                 | SpeedL   Env Mutator StatLValue BoardMonsterVar LazyInt

public export
data SkillComponent = SkillComponent_ (List SkillEffect, Maybe (Condition, SkillComponent, SkillComponent, SkillComponent)) String {-the string here is the temporaryId of the player in the game-}




{-the following could probably be cleaned up a lot as well....-}


{-executeSkillEffectTransform : SkillEffect -> BasicMonster -> Maybe BasicMonster {-Nothing indicates a logic error-} {-This function already exists 
-}

in objects_advanced, but handles integration with the game, etc.-} {-Probably this needs to go-}
{-executeSkillEffectTransform (AttackL env mutator statLValue boardMonsterVar lazyInt) basicMonster with (mutator, statLValue, )-}






executeSkillEffectTransform (SkillEffectStatL AttackL env mutator statLValue (UnBoundBoardMonsterVar _ _) lazyInt) _ = Nothing

-}


public export
getIndexOfMonster' : Nat -> Board -> Maybe (Fin 9)
getIndexOfMonster' nat board = head' (findIndices (\m => True) board)

public export
getIndexOfMonster : Player -> BoardMonsterVar -> Maybe (Fin 9)
getIndexOfMonster _ (UnBoundBoardMonsterVar _ _) = Nothing
getIndexOfMonster player (BoundBoardMonsterVar nat) = getIndexOfMonster' nat (board player)




public export
executeSkillEffect : Game -> (player : Player) -> (opponent : Player) -> SkillEffect -> (Game, List ClientUpdate)
executeSkillEffect game player opponent (SkillEffectStatL skillEffectStatType env mutator statLValue boardMonsterVar lazyInt) with (getIndexOfMonster player boardMonsterVar)
 | Nothing = (game, [GameLogicError])
 | Just indexOfMonster with (skillEffectStatType)
  | AttackL  = ?g
  | DefenseL = ?g
  | RangeL   = ?g
  | LevelL   = ?g
  | SpeedL   = ?g



{-I might already have a hole by this name-}



{-
public export
executeSkillEffect : Game -> SkillEffect -> (Game, List ClientUpdate)
executeSkillEffect game skillEffect with (skillEffect)
 |AttackL env mutator statLValue boardMonsterVar lazyInt = (game,[])
 |_ = (game,[])

-}



{-One of the things I need to do here is have functionality for getting the owner player and opponent player-}


{-This should use executeSkillEffectTransform to replace the monster affected in the game-}













{-I'm using tokens to identify players. I should abstract this. It's okay if it's the same data but I should not depend on this being the case.-}
{-This is particularly the case as I'll probably store the player identifier in skills to keep track of the owner. This means that when a player reconnects they need to still be matched even if they have a new token, etc-}

public export
record Battle where
 constructor MkBattle
 round                : Round
 originalPlayerAToken : String
 originalPlayerBToken : String
 game                 : Game

public export
getPlayer : Game -> WhichPlayer -> Player
getPlayer game PlayerA = player_A game
getPlayer game PlayerB = player_B game

public export
opponent : WhichPlayer -> WhichPlayer
opponent PlayerA = PlayerB
opponent PlayerB = PlayerA








