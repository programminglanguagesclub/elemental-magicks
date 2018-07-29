module Main.Game
import Data.Fin
import Data.Vect
import Base.Utility
import Base.Bounded
import Base.Hp
import Base.Preliminaries
import Base.BoundedList
import Base.Objects_basic
import Base.Skill_dsl_data
import Base.Skill_dsl_logic
import Base.Player
import Base.Phase
import Base.Card
import Base.Clientupdates
import Pruviloj.Derive.DecEq

%access public export
%default total

%language ElabReflection
-------------------------------------------------------------------------------
data Round
 = FirstRound
 | SecondRoundOriginalPlayerAWonFirstRound
 | SecondRoundOriginalPlayerBWonFirstRound
-------------------------------------------------------------------------------
{-Reset used_death_skill, used_counter_skill before START OF PHASE?------auto skill and action of card. -}

-------------------------------------------------------------------------------
-- monster id data type?

-------------------------------------------------------------------------------
record Game where
 constructor MkGame
 --initiative : WhichPlayer
 turnNumber : Nat
 skillHead : Maybe (Nonautomatic, Fin 25, WhichPlayer, Env) -- evokerId, whichPlayer
 skillQueue : List (Skill, Fin 25, WhichPlayer, SkillType) -- skill, evokerId, whichPlayer, skillType
 deathQueue : List (Fin 25, WhichPlayer)

 phase : Phase
 playerA : Player
 playerB : Player
 playerOnMove : WhichPlayer
-------------------------------------------------------------------------------
--correctGame : Game -> Bool
---
--correctGame game with (turnNumber game = Z)
-- | Yes prf = True
-- | No prf = False
-------------------------------------------------------------------------------

getNumberFielded : Player -> Nat
getNumberSpawning : Player -> Nat -- either 0 or 1
getFieldedIds : Player -> List (Fin 25)
getSpawningIds : Player -> List (Fin 25)

getPlayerIdList : Player -> List (Fin 25)
getPlayerIdList player = (map Base.Card.getId $ hand player) ++ (map Base.Card.getId $ graveyard player) ++ (map Base.Card.getId $ discard player) ++ (getFieldedIds player) ++ (getFieldedIds player)


getAllEnvironmentVariables : Env -> List String
getAllReferencedVariables : Nonautomatic -> List String


-- encode the fact that all variables are bound to their environment

data CorrectPlayer : Player -> Type where
 MkCorrectPlayer :
  (player : Player) ->
  (length $ getPlayerIdList player = 25) ->
 -- ((Base.Card.getId <$> (index' 0 $ hand player)) = Just FZ) ->
  (UniqueList $ getPlayerIdList player) ->
  ((x : Fin 25) -> find (==x) (getPlayerIdList player) = Nothing -> Void) ->
  CorrectPlayer player

data CorrectGame : Game -> Type where
 MkCorrectGame :
  (game : Game) ->
  --turnNumber game = Z ->
  CorrectPlayer (playerA game) ->
  CorrectPlayer (playerB game) ->
  CorrectGame game


-- have to do appending on the other side, etc...
{-
errorPreserved : (l : List (Fin 25)) -> (m : List (Fin 25)) -> (UniqueList l -> Void) -> UniqueList (l ++ m) -> Void
errorPreserved l m prfLNotUnique prfLMUnique with (l)
  | [] = prfLNotUnique UniqueEmpty
  | (lh::lt) with (prfLNotUnique)
   | 
   -}

g : a = b -> b = a


   
uniqueVect : Vect n (Fin 25) -> Bool
uniqueVect [] = True
uniqueVect (x::xs) with (find (==x) xs)
 | Nothing = uniqueVect xs
 | Just _ = False
                   
data UniqueVect : Vect n (Fin 25) -> Type where
 UniqueEmpty : UniqueVect []
 UniqueConcat : UniqueVect xs -> find (==x) xs = Nothing -> UniqueVect (x::xs)
 
notUniqueHead : (x : (Fin 25)) -> (xs : Vect n (Fin 25)) -> (find (==x) xs = Nothing -> Void) -> UniqueVect (x::xs) -> Void
notUniqueHead x xs inTail UniqueEmpty impossible
notUniqueHead x xs inTail (UniqueConcat uniqueTail notInTail) = inTail notInTail
 
notUniqueTail : (x : (Fin 25)) -> (xs : Vect n (Fin 25)) -> (UniqueVect xs -> Void) -> UniqueVect (x::xs) -> Void
notUniqueTail x xs tailNotUnique UniqueEmpty impossible
notUniqueTail x xs tailNotUnique (UniqueConcat uniqueTail notInTail) = tailNotUnique uniqueTail
 
decUniqueVect : (l : Vect n (Fin 25)) -> Dec (UniqueVect l)
decUniqueVect [] = Yes UniqueEmpty
decUniqueVect (x::xs) with (decEq (find (==x) xs) Nothing)
 | Yes headNotInTail with (decUniqueVect xs)
  | Yes uniqueTail = Yes (UniqueConcat uniqueTail headNotInTail)
  | No tailNotUnique = No (notUniqueTail x xs tailNotUnique)
 | No headInTail = No (notUniqueHead x xs headInTail)
 
q : plus n 0 = n


appendNilRightNeutral : (l : Vect n a) -> l ++ [] = l
appendNilRightNeutral [] = Refl
appendNilRightNeutral (x::xs) =
 let inductiveHypothesis = appendNilRightNeutral xs in
 rewrite inductiveHypothesis in Refl



--Philip!!!!

philipCast : {x : a} -> {y : a} -> (x = y) -> (f : a -> Type) -> f x -> f y
philipCast e f b = replace e b {P = f}


{-
cast (_ : n = plus n 0) (\n -> Vect n a)
is what you want
it might be clearer to write 
cast (_ : n = plus n 0) (\x -> Vect x a)




cast (_ : x = y) (\t -> f t = f x) refl
is your proof that f y = f x

-}

gh : (l : Vect n (Fin 25)) -> UniqueVect l = UniqueVect (l ++ []) 
hg : (l : Vect n (Fin 25)) -> UniqueVect l = UniqueVect ([] ++ l)
hg l = Refl

--gah : UniqueVect l = UniqueVect (l ++ [])
--gah = philipCast f ?hole ?hole
{-
ges : (n : Nat) -> Type
ges n = Vect n (Fin 25)


blara : Vect n (Fin 25) = Vect (plus n 0) (Fin 25)
blara = philipCast q ges Refl

-}


-- this worked... then just suddenly stopped working...!!
{-
uniqueConcat2 : (l : Vect n (Fin 25)) -> (k : Vect m (Fin 25)) -> UniqueVect (l ++ k) -> UniqueVect l
uniqueConcat2 l [] lkUnique = rewrite gh l in lkUnique -----rewrite (g $ appendNilRightNeutral l) in (rewrite (gh l) in lkUnique) -- put {n=n}{m=m} into scope?
                              -}



--uniqueConcat l (kh::kt) lkUnique with (lkUnique)
--  uniqueConcat {n=n} {m=Z} l (kh::kt)   | UniqueEmpty = ?hole --impossible
-- uniqueConcat {n=n} {m=S m'} l (kh::kt) | UniqueConcat lktUnique lkhUnique = ?hole --uniqueConcat l kh lktUnique




uniqueConcat : (l : Vect n (Fin 25)) -> (k : Vect m (Fin 25)) -> UniqueVect (k ++ l) -> UniqueVect l
uniqueConcat l [] klUnique = rewrite hg l in klUnique
uniqueConcat l (kh::kt) klUnique with (klUnique)
  | UniqueEmpty impossible
  | UniqueConcat uniqueListT uniqueH = uniqueConcat l kt uniqueListT


notUniqueConcat : (l : Vect n (Fin 25)) -> (k : Vect m (Fin 25)) -> (UniqueVect l -> Void) -> UniqueVect (k++l) -> Void
notUniqueConcat l k notUniqueL uniqueKL = notUniqueL $ uniqueConcat l k uniqueKL



{-

Type mismatch between
        UniqueVect (x :: xs) (Type of UniqueConcat lktUnique lkhUnique)
        and
                UniqueVect (l ++ kh :: kt) (Expected type)

                Specifically:
                        Type mismatch between
                                        x :: xs
                                                and
                                                                l ++ kh :: kt


                                                                -}


{-

uniqueList : List (Fin 25) -> Bool
uniqueList [] = True
uniqueList (x::xs) with (Prelude.List.find (==x) xs)
   | Nothing = uniqueList xs
      | Just _ = False
         
          
          data UniqueList : List (Fin 25) -> Type where
               UniqueEmpty : UniqueList []
                  UniqueConcat : UniqueList xs -> Prelude.List.find (==x) xs = Nothing -> UniqueList (x::xs)
                   
                   notUniqueHead : (x : (Fin 25)) -> (xs : List (Fin 25)) -> (find (==x) xs = Nothing -> Void) -> UniqueList (x::xs) -> Void
                   notUniqueHead x xs inTail UniqueEmpty impossible
                   notUniqueHead x xs inTail (UniqueConcat uniqueTail notInTail) = inTail notInTail
                    
                    notUniqueTail : (x : (Fin 25)) -> (xs : List (Fin 25)) -> (UniqueList xs -> Void) -> UniqueList (x::xs) -> Void
                    notUniqueTail x xs tailNotUnique UniqueEmpty impossible
                    notUniqueTail x xs tailNotUnique (UniqueConcat uniqueTail notInTail) = tailNotUnique uniqueTail
                     
                      
                      decUniqueList : (l : List (Fin 25)) -> Dec (UniqueList l)
                      decUniqueList [] = Yes UniqueEmpty
                      decUniqueList (x::xs) with (decEq (find (==x) xs) Nothing)
                        | Yes headNotInTail with (decUniqueList xs)
                              | Yes uniqueTail = Yes (UniqueConcat uniqueTail headNotInTail)
                                  | No tailNotUnique = No (notUniqueTail x xs tailNotUnique)
                                    | No headInTail = No (notUniqueHead x xs headInTail)



-}



{-
data MonsterLocation
 = HandLocation (Fin 25)
 | GraveyardLocation (Fin 25)
 | BanishLocation (Fin 25)
 | BoardLocation (Fin 9)
 | SpawnLocation

monsterLocationDecEq : (x , y : MonsterLocation) -> Dec (x = y)
%runElab deriveDecEq `{monsterLocationDecEq}

DecEq MonsterLocation where
  decEq x y = monsterLocationDecEq x y
-------------------------------------------------------------------------------
data SpellLocation
 = HandLocationSpell (Fin 25)
 | GraveyardLocationSpell (Fin 25)
 | BanishLocationSpell (Fin 25)
 | SpawnLocationSpell

spellLocationDecEq : (x , y : SpellLocation) -> Dec (x = y)
%runElab deriveDecEq `{spellLocationDecEq}
   
DecEq SpellLocation where
  decEq x y = spellLocationDecEq x y
  -}


  
-------------------------------------------------------------------------------
data CardLocation
 = HandLocation (Fin 25)
 | GraveyardLocation (Fin 25)
 | BanishLocation (Fin 25)
 | BoardLocation (Fin 9)
 | SpawnLocation


lookUpCardId : CardLocation -> Fin 25


{-

-- these are not all monsters potentially as there are also spell cards.
data MonsterDictionary =
 MkMonsterDictionary
  (DPair
   (Vect m MonsterLocation, Vect n SpellLocation)
   (\(monsterLocationDictionary, spellLocationDictionary) =>
    (i : Fin m) ->
    (j : Fin m) ->
    (i = j -> Void) ->
    (index i monsterLocationDictionary = index j monsterLocationDictionary) ->
    Void))



-}
-------------------------------------------------------------------------------
namespace fromNat
  getInitiative : Nat -> WhichPlayer
  getInitiative turnNumber =
   case even turnNumber of
    True => PlayerB
    False => PlayerA
-------------------------------------------------------------------------------
namespace fromGame
  getInitiative : Game -> WhichPlayer
  getInitiative game = getInitiative $ turnNumber game
-------------------------------------------------------------------------------
pushSkill' :
 (Skill, Fin 25, WhichPlayer, SkillType) ->
 List (Skill, Fin 25, WhichPlayer, SkillType) ->
 List (Skill, Fin 25, WhichPlayer, SkillType)

pushSkill' skill skillQueue = skillQueue ++ [skill]
-------------------------------------------------------------------------------
pushSkill : (Skill, Fin 25, WhichPlayer, SkillType) -> Game -> Game
pushSkill skill game = record {skillQueue $= pushSkill' skill} game
-------------------------------------------------------------------------------
record DrawPhase where
 constructor MkDrawPhase
 playerA : DrawPlayer
 playerB : DrawPlayer
 cardsDrawn : Fin 60
-------------------------------------------------------------------------------
data FullGame
 = MkFullGameGame (game : Game ** CorrectGame game)
 | MkFullGameDrawPhase DrawPhase
-------------------------------------------------------------------------------
initialTurnNumber : Nat
initialTurnNumber = S Z
-------------------------------------------------------------------------------
initialSkillHead : Maybe (Nonautomatic, Fin 25, WhichPlayer, Env)
initialSkillHead = Nothing
-------------------------------------------------------------------------------
initialSkillQueue : List (Skill, Fin 25, WhichPlayer, SkillType)
initialSkillQueue = []
-------------------------------------------------------------------------------
initialDeathQueue : List (Fin 25, WhichPlayer)
initialDeathQueue = []
-------------------------------------------------------------------------------
initialPhase : Phase
initialPhase = SpawnPhase
-------------------------------------------------------------------------------
newGame :
 String ->
 Vect 5 SoulCard ->
 Vect 25 Card ->
 Nat ->
 String ->
 Vect 5 SoulCard ->
 Vect 25 Card ->
 Nat ->
 Game

newGame aId aHand aSoul aRemainingTime bId bHand bSoul bRemainingTime =
 MkGame
  initialTurnNumber
  initialSkillHead
  initialSkillQueue
  initialDeathQueue
  initialPhase
  (newPlayer aId aHand aSoul $ aRemainingTime + timeIncrementMilliseconds) -- not sure if I should increment here
  (newPlayer bId bHand bSoul $ bRemainingTime + timeIncrementMilliseconds) -- not sure if I should increment here
  PlayerA
-------------------------------------------------------------------------------
initialCardsDrawn : Fin 60
initialCardsDrawn = 0
-------------------------------------------------------------------------------
newDrawPhase :
 String ->
 String ->
 DrawPhase

newDrawPhase aId bId =
 MkDrawPhase (newDrawPlayer aId) (newDrawPlayer bId) initialCardsDrawn
-------------------------------------------------------------------------------
record Battle where
 constructor MkBattle
 round : Round
 game : FullGame
-------------------------------------------------------------------------------
namespace game
  getPlayer :
   WhichPlayer ->
   Game ->
   Player

  getPlayer PlayerA game = playerA game
  getPlayer PlayerB game = playerB game
-------------------------------------------------------------------------------
namespace draw
  getPlayer :
   WhichPlayer ->
   DrawPhase ->
   DrawPlayer

  getPlayer PlayerA drawPhase = playerA drawPhase
  getPlayer PlayerB drawPhase = playerB drawPhase
-------------------------------------------------------------------------------
namespace foo
  getStatefulPlayer :
   WhichPlayer ->
   Game ->
   (Player, Player -> Game -> Game)

  getStatefulPlayer PlayerA game = (playerA game, \player => \game => record {playerA = player} game)
  getStatefulPlayer PlayerB game = (playerB game, \player => \game => record {playerB = player} game)
-------------------------------------------------------------------------------
getPlayerMutator :
 WhichPlayer ->
 Player ->
 Game ->
 Game

getPlayerMutator PlayerA player game = record {playerA = player} game
getPlayerMutator PlayerB player game = record {playerB = player} game
-------------------------------------------------------------------------------
getPlayerTemporaryId :
 WhichPlayer ->
 FullGame ->
 String

getPlayerTemporaryId whichPlayer (MkFullGameGame (game ** correctGame)) = temporaryId $ getPlayer whichPlayer game
getPlayerTemporaryId whichPlayer (MkFullGameDrawPhase drawPhase) = temporaryId $ getPlayer whichPlayer drawPhase
-------------------------------------------------------------------------------
transformPlayer :
 (Player -> Player) ->
 WhichPlayer ->
 Game ->
 Game

transformPlayer mutator PlayerA game = record {playerA $= mutator} game
transformPlayer mutator PlayerB game = record {playerB $= mutator} game
-------------------------------------------------------------------------------
{-
updatePlayer :
 WhichPlayer ->
 Game ->
 (Player -> (Player, List ClientUpdate)) ->
 (Game, List ClientUpdate)

updatePlayer whichPlayer game mutator =
 let game' = transformPlayer (fst . mutator) whichPlayer game in ?hole
 -}
-------------------------------------------------------------------------------
-- Move these helper functions to card. These also get used in the DSL.

modifyHp :
 (Integer -> Integer) ->
 FieldedMonster ->
 FieldedMonster

modifyHp mutator monster =
 record {basic -> hp $= transformHp mutator} monster
-------------------------------------------------------------------------------
subtractHp :
 Nat ->
 FieldedMonster ->
 (Bool, FieldedMonster)

subtractHp value monster with (value)
 | Z = (False, monster)
 | S _ = (True, modifyHp (\x => x - (toIntegerNat value)) monster)
-------------------------------------------------------------------------------
fatallyDamaged : FieldedMonster -> Bool
fatallyDamaged monster = (getCurrentHp $ hp $ basic monster) <= 0
-------------------------------------------------------------------------------
{-
transformMonsterInGame :
 Fin 3 ->
 Fin 3 ->
 WhichPlayer ->
 (FieldedMonster -> FieldedMonster) ->
 Game ->
 Game

transformMonsterInGame row column whichPlayer monsterMutator game =
 let playerMutator = transformMonster monsterMutator row column in
 transformPlayer playerMutator whichPlayer game

-------------------------------------------------------------------------------
damageCard :
 Nat ->
 Fin 3 ->
 Fin 3 ->
 Game ->
 WhichPlayer ->
 (List ClientUpdate, Game)


damageCard damage row column game whichPlayer with (indexMonster row column (the Player ?defendingPlayer))
 | Nothing = ([], game)
 | Just monster =
  let defenderDefense = removeUpperBound $ getTemporary $ defense $ basic monster in
  let hpLost = minus damage defenderDefense in
  let (fatallyDamaged, damagedMonster) = subtractHp hpLost monster in
  let damagedCardUpdate = the ClientUpdate ?hole in -- do not kill the card at this point.
  let transformDefender = transformMonsterInGame row column whichPlayer in
  let game' = transformDefender (\m => damagedMonster) game in
  case fatallyDamaged of
   True =>
    case getCanUseDeathSkill damagedMonster of
     Nothing => ([damagedCardUpdate], game')
     Just skill =>
      --let game'' = transformDefender (setCanUseDeathSkill False) game' in
      let game'' = pushSkill skill game' in
      -- Because the death skill might not activate (its conditions are checked before it is loaded onto the skill head)
      -- we cannot actually do setCanUseDeathSkill False yet.
      -- do I want to remove death skills from the queue if the unit is healed to hp > 0?
      ([damagedCardUpdate], game'')
   False =>
    case getCanUseCounterSkill damagedMonster of
     Nothing => ([damagedCardUpdate], ?hole)
     Just skill =>
      --let game'' = transformDefender (setCanUseCounterSkill False) game' in
      let game'' = pushSkill skill game' in
      ([damagedCardUpdate], game'')

  {- if hp > 0, and has counter skill, then see if counter skill has been used. otherwise same with death skill. -}


-- this function probably needs to be able to modify more things..
-}
-------------------------------------------------------------------------------
{-applyAttack :
 Bounded 0 Preliminaries.absoluteUpperBound ->
 Fin 3 ->
 Player ->
 List Skill ->
(List ClientUpdate, Player, List Skill)

applyAttack atk row defendingPlayer = ?hole

-- this function also probably needs to be able to modify more things.
-}
-------------------------------------------------------------------------------
