module Player
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
%access public export
%default total

Skill : Type
Skill = (Automatic, Bool, Nat)


{- does a skill exist, and has it been used? This way I can also easily make it so that spell cards don't disappear as soon as they are used and monsters don't become engaged as they use their action skills-}

{- used : Bool, cost : Nat -}

record Monster where
 constructor MkMonster
 basic : BasicMonster
 startSkill : Maybe Skill
 endSkill : Maybe Skill
 counterSkill : Maybe Skill
 spawnSkill     : Maybe Skill
 deathSkill      : Maybe Skill
 autoSkill   : Maybe Skill
 actionSkills : List Skill

record Spell where
 constructor MkSpell
 basic      : BasicSpell
 spawnSkill : Skill

data Card = SpellCard Spell | MonsterCard Monster


{-
syntax spell [basic] [spawn] = MkSpell basic spawn
-}
{-
mutant_pig : BasicMonster
mutant_pig = basic monster 20 0 2 1 3
-}




{-
public export
Board : Type
Board = Vect 9 (Maybe Monster)
-}
Spawn : Type
Spawn = Maybe Card
Soul : Type
Soul = Vect 5 (Maybe Monster) {- again more information could go in the type -}


{- unused? -}
Thoughts : Type
Thoughts = Bounded 0 absoluteUpperBound

{-this might go in preliminaries-}
{-
public export
transformThoughts : (Integer -> Integer) -> Thoughts -> Thoughts
transformThoughts = transformBounded 0 absoluteUpperBound Oh Oh
-}

record Player where
 constructor MkPlayer
 board : Vect 9 (Maybe Monster)
 rowTarget : Vect 3 (Fin 3)
 hand : List Card
 graveyard : List Card
 discard : List Card
 spawn : Spawn
 soul : Soul
 thoughts : Bounded 0 Preliminaries.absoluteUpperBound
 knowledge : Vect 6 (Bounded 0 9)
 temporaryId : String

getLiving : Maybe Monster -> Bool
getLiving Nothing = False
getLiving (Just m) with (aliveness (basic m))
 | Alive = True
 | DeadFresh = False
 | DeadStale = False



{-getRowTarget : Player -> Fin 3 -> Maybe Nat {-This takes a player and a row, and returns the index of the next target, or nothing if there is no valid target (living monster)-}-}

{-
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
 -}

getNextRowTarget : Fin 3 -> Fin 3
getNextRowTarget FZ = FS FZ
getNextRowTarget (FS FZ) = FS (FS FZ)
getNextRowTarget (FS (FS FZ)) = FZ
getNextRowTarget (FS (FS (FS FZ))) impossible
getNextRowTarget (FS (FS (FS (FS _)))) impossible




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




syntax "new" "player" [token] = MkPlayer (Vect.replicate 9 Nothing) [FZ,FZ,FZ] [] [] [] Nothing (Vect.replicate 5 Nothing) (>> 0 <<) (Vect.replicate 6 (>> 0 <<)) token

