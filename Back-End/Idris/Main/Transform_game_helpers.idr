module Main.Transform_game_helpers
import Data.Vect
import Base.Bounded
import Base.Preliminaries
import Base.Objects_basic
import Base.Phase
import Base.Player
import Base.Card
import Main.Game
import Base.Clientupdates
%access public export
%default total

-------------------------------------------------------------------------------
schoolsHighEnoughToPlayCard :
 Player ->
 Card ->
 Bool

schoolsHighEnoughToPlayCard player (SpellCard card) =
 geq (index (school $ basic card) $ knowledge player) (level $ basic card)

schoolsHighEnoughToPlayCard player (MonsterCard card) with (schools (basic card))
 | NoSchools = True
 | OneSchool s =
  geq (index s $ knowledge player) (snd $ snd $ level $ basic card)
 | TwoSchools s1 s2 =
  geq (index s1 $ knowledge player) (snd $ snd $ level $ basic card) &&
  geq (index s2 $ knowledge player) (snd $ snd $ level $ basic card)
-------------------------------------------------------------------------------



{-

  findWithIndexPreferentiallyFrom :
   DecEq a =>
   (p : a -> Bool) ->
   (begin : Fin (S n)) ->
   (v1 : Vect (S n) a) ->
   Either (find p v1 = Nothing) (DPair (Fin (S n), a) (\(i1,e1) => (Vect.index i1 v1 = e1, ((i2 : Fin (S n)) -> (So (p (Vect.index i2 v1))) -> leq2 $ compare (computeSearchIndex begin i1) (computeSearchIndex begin i2) = True))))


-}

-------------------------------------------------------------------------------
standardEngagementPriority : (Monster, Fin n) -> (Integer, Fin n)
standardEngagementPriority (monster, boardIndex) = (removeBounds $ fst $ speed $ basic monster, boardIndex)

getMonstersWithPositions : Vect n (Maybe a) -> Vect n (Maybe a, Fin n) -- store your index
getMonstersWithPositions = ?hole
-------------------------------------------------------------------------------
reindex :
 Ord b =>
 Vect n (Maybe a) ->
 (engagementPriority : (a, Fin n) -> b) ->
 DPair                                            -- DATA
  (Vect n (Maybe a, Fin n))                       -- Fin n indexes old vector
--------------------------------------------------
  (\v => (                                        -- SPECIFICATION
    (i : Fin n) ->
    (j : Fin n) -> (
     index i v = (Just iElement, iOriginalIndex), -- given two actual elements
     index j v = (Just jElement, jOriginalIndex),
     (prf : compare i j = LT) ->                  -- if i < j in result vector
      compare
       (engagementPriority (iElement, iOriginalIndex))
       (engagementPriority (jElement, jOriginalIndex)) = GT ->
      Void                                        -- cards @ i <= j in priority
  )))

reindex board engagementPriority = -- is this even provable given present generality?
 let smartBoard = getMonstersWithPositions board in
 ?hole

-------------------------------------------------------------------------------

-- just gets for single player...

validForAction : Maybe Monster -> Maybe (Monster, Fin n)
-- says if a card exists, is alive, and is disengaged.
{-
isValidForAction : Maybe Monster -> Bool
isValidForAction m with (validForAction m)
 | Just _ = True
 | Nothing = False

-}

----- SHOULD VALIDITY CONDITION RETURN MAYBE INTEGER (THE SPEED)?????

compareMonsterPriority : (square_speed_1 : (Fin n, Integer)) -> (square_speed_2 : (Fin n, Integer)) -> Ordering
compareMonsterPriority (square1, speed1) (square2, speed2) with (compare speed1 speed2)
 | LT = LT
 | EQ = compare square1 square2
 | GT = GT

-- define a function for getting either the highest priority monster next or a proof that there is no monster that is alive and disengaged.


priority : (Monster, Fin n) -> (Integer, Fin n) -- have to invert speed so this works. smaller here gives higher priority.

integerSpeed : Monster -> Integer
integerSpeed monster = extractBounded $ getTemporary $ speed $ basic monster
-------------------------------------------------------------------------------
doubleIndex : Vect n a -> Vect n a -> Bool -> Fin n -> a
doubleIndex x y True i = index i x
doubleIndex x y False i = index i y
-------------------------------------------------------------------------------
--UNTRUSTED COMPONENT
-------------------------------------------------------------------------------
incrementIndex : Either a (Monster, Fin n) -> Either a (Monster, Fin (S n))
incrementIndex (Left x) = Left x
incrementIndex (Right (m, k)) = Right (m, FS k)


-------------------------------------------------------------------------------
-- assumes one flat list which is interleaved with initiative first.
-- also need to check disengagement and aliveness.... (not doing that currently....)

valid : Maybe Integer -> Bool
valid Nothing = False
valid (Just _) = True


getNext :
 (initiativeBoard : Vect n (Maybe Monster)) ->
 (otherBoard : Vect n (Maybe Monster)) ->
 (validityCondition : Maybe Monster -> Maybe Integer) ->
 Either
  (Data.Vect.find (Main.Transform_game_helpers.valid . validityCondition) initiativeBoard = Nothing,
   Data.Vect.find (Main.Transform_game_helpers.valid . validityCondition) otherBoard = Nothing)
  (Monster, Fin n)

getNext [] [] _ = Left (Refl, Refl)
getNext (x::xs) (y::ys) validityCondition with (validityCondition x, validityCondition y)
 | (Nothing, Nothing) with (getNext xs ys validityCondition)
   | Left(_,_) = ?hole ----Left (Refl, Refl) = ?hole ----Left (Refl, Refl)
   | Right (monster, i) = ?term
 | (Nothing, Just speed2) = ?term
 | (Just speed1, Nothing) = ?term
 | (Just speed1, Just speed2) = ?term


{-
getNext (x::xs) validityCondition with (validityCondition x)
  | True with (incrementIndex $ getNext xs validityCondition)
    | Left _ = Right (x, FZ)
    | Right (tailMonster, tailIndex) with (compare (integerSpeed monster) (integerSpeed tailMonster))
     | LT = Right (tailMonster, tailIndex)
     | EQ = Right (monster, FZ)
     | GT = Right (monster, FZ)
  | False = incrementIndex $ getNext xs validityCondition
  -}
-------------------------------------------------------------------------------
{-
getNext [] = Nothing
getNext (Nothing::xs) = incrementIndex $ getNext xs
getNext ((Just monster)::xs) with (incrementIndex $ getNext xs)
  | Nothing = Just (monster, FZ)
  | Just (tailMonster, tailIndex) with (compare (integerSpeed monster) (integerSpeed tailMonster))
   | LT = Just (tailMonster, tailIndex)
   | EQ = Just (monster, FZ)
   | GT = Just (monster, FZ)
   -}
-------------------------------------------------------------------------------
buildFlattenedVector :
 Vect n (Maybe Monster) ->
 Vect n (Maybe Monster) ->
 Vect (n + n) (Maybe Monster)

buildFlattenedVector [] [] = []
buildFlattenedVector {n=S n} (x::xs) (y::ys) =
 rewrite pullOutPlus n n in x::y::(buildFlattenedVector xs ys)
-------------------------------------------------------------------------------
getNextMonsterSimplyTyped' :
 Vect 9 (Maybe Monster) ->
 Vect 9 (Maybe Monster) ->
 Maybe (Monster, Fin 18)

getNextMonsterSimplyTyped' initiativeBoard otherBoard = ?term
 --getNext (buildFlattenedVector initiativeBoard otherBoard)
-------------------------------------------------------------------------------
getNextMonster :
 (initiativePlayerBoard : Vect 9 (Maybe Monster)) ->
 (otherPlayerBoard : Vect 9 (Maybe Monster)) ->
 (validityCondition : Maybe Monster -> Bool) ->
 Either
  (find validityCondition initiativePlayerBoard = Nothing,
   find validityCondition otherPlayerBoard = Nothing)
  (DPair
   (Monster, Fin 9, Bool)
   (\(monster, i, whichBoard) => (
    doubleIndex initiativePlayerBoard otherPlayerBoard whichBoard i = Just monster,
    (otherIndex : Fin 9) ->
    (otherWhichBoard : Bool) ->
    (otherMonster : Monster) ->
    doubleIndex initiativePlayerBoard otherPlayerBoard otherWhichBoard otherIndex = Just otherMonster ->
    ((otherIndex, otherWhichBoard) = (i,whichBoard) -> Void) ->
    compareMonsterPriority (i, extractBounded $ getTemporary $ speed $ basic monster) (otherIndex, extractBounded $ getTemporary $ speed $ basic otherMonster) = GT)))
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------




























engagementPhaseOnMove :
 (board : Vect n (Maybe Monster)) ->
 Either
  (Fin n -> validForAction $ index i board = Nothing)
  (DPair
   (Monster, Fin n)
   (
    \(monster, square) => (
     validForAction $ index square board = Just (monster,square),
     (otherMonster : Monster) -> 
     (otherSquare : Fin n) ->
     validForAction $ index otherSquare board = Just (otherMonster, otherSquare) ->
     (otherSquare = square -> Void) ->
     (compare (priority (monster,square)) (priority (otherMonster, otherSquare))) = LT
    )
   )
  )

findLargest : Vect n a -> (a -> Integer) -> a
findLargest v f = ?hole


engagementPhaseOnMove board =
 let newVect = map validForAction board in ?hole
 



-------------------------------------------------------------------------------
{-For now, completely ignore the possibility of the user using skills! :D -}
{-
_engagementOnMove :
 (playerABoard : Vect 9 (Maybe Monster)) ->
 (playerBBoard : Vect 9 (Maybe Monster)) ->
 (initiative : WhichPlayer) ->
 (WhichPlayer, Fin 9)

_engagementOnMove playerABoard playerBBoard initiative = ?hole
 --let playerAUnit = findWithIndexPreferentiallyFrom ?hole ?hole ?hole in ?hole
-}
-------------------------------------------------------------------------------
{-engagementOnMove :
 (game : Game) ->
 (player : Player) ->
 (opponent : Player) ->
 (Bool, Fin 9)
-}

-------------------------------------------------------------------------------



{-could return a maybe nat, where nothing indicates an error, but I'll trust the ability to not have it the engagement phase if there's nothing next to move-}


-------------------------------------------------------------------------------

_getEnemyStuffInFront : (defenderBoard) -> (row : Fin 3) -> Nat
_getFriendlyStuffInFront : (attackerBoard : Vect 9 (Maybe Monster)) -> (attackerSquare : Fin 9) -> Nat 
inRangeRow : (attackerBoard : Vect 9 (Maybe Monster)) -> (defenderBoard : Vect 9 (Maybe Monster)) -> (attackerSquare : Fin 9) -> (row : Fin 3) -> Maybe Bool
inRangeRow attackerBoard defenderBoard attackerSquare row with (index attackerSquare attackerBoard)
 | Nothing = Nothing
 | Just monster with (aliveness (basic monster))
  | DeadFresh = Nothing
  | DeadStale = Nothing
  | Alive with (range (basic monster))
   | (temporaryRange,_,_) = if gt (fromIntegerNat (extractBounded temporaryRange)) ((_getFriendlyStuffInFront attackerBoard attackerSquare) + (_getEnemyStuffInFront defenderBoard row)) then Just True else Just False


-------------------------------------------------------------------------------
moveUnit :
 (moveFrom : Fin 9) ->
 (moveTo : Fin 9) ->
 (board : Vect 9 (Maybe Monster)) ->
 Vect 9 (Maybe Monster) {-this actually does a swap-}


moveUnit moveFrom moveTo board =
 let to = index moveTo board in
 replaceAt moveFrom to (replaceAt moveTo (index moveFrom board) board)
-------------------------------------------------------------------------------


-- take proof that
-- 2) Unit is on move
-- 3) Square is correct for unit
-- 1) Oh, and it's the engagement phase

-- return proof that
-- ??

{-
implementation DecEq (Bool -> Bool) where
 decEq f g with (decEq (f True) (g True), decEq (f False) (g False))
  | (Yes _, Yes _) = Yes $ the (f=g) $ believe_me Refl
  | (Yes _, No _) = No ?hole
  | (No _, Yes _) = No ?hole
  | (No _, No _) = Yes $ the (f=g) $ believe_me Refl
-}
restUnit :
 (location : Fin 9) -> -- data
 (game : Game) -> -- data
 (whichPlayer : WhichPlayer) -> -- data
 (phase game = EngagementPhase) -> -- refinement
 () -> -- refinement
 () -> -- refinement
 (Game,List ClientUpdate)

-------------------------------------------------------------------------------
_getHandCards : (hand : List Card) -> (acc : MultiTree Nat) -> MultiTree Nat
_getHandCards [] acc = acc
_getHandCards (card::cards) acc with (card)
 |MonsterCard m      = _getHandCards cards (insert acc (id (basic m)))
 |SpellCard s        = _getHandCards cards acc
-------------------------------------------------------------------------------
getHandCards : (hand : List Card) -> MultiTree Nat
getHandCards hand = _getHandCards hand Leaf
-------------------------------------------------------------------------------

