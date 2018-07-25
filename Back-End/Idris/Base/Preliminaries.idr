module Base.Preliminaries
import Data.Vect
import Data.So
import Base.Bounded
import Pruviloj.Derive.DecEq

%access public export
%default total

%language ElabReflection

{- this appears to not work, perhaps because lhs will not be the same lhs as in program. Oh well..
syntax [lhs] ":=" [rhs] ";" [program] = let lhs = rhs in program
-}


-- should have unique list, unique vector, etc, I guess. 
-- this can be generalized to traversable.



-- implementation Show elem => Show (Vect len elem) where
--      show = show . toList


implementation Show (Fin n) where
 show = show . finToNat




initialTimeRemainingMilliseconds : Nat
initialTimeRemainingMilliseconds = 900000 -- 15 minutes

timeIncrementMilliseconds : Nat -- per action
timeIncrementMilliseconds = 5000

-- for some reason did not have to declare Eq or DecEq on a.
data UniqueVector : Nat -> Type -> Type where
 MkUniqueVector :
  (v : Vect n a) ->
  ((i : Fin n) ->
   (j : Fin n) ->
   (i = j -> Void) ->
   index i v = index j v ->
   Void) ->
  UniqueVector n a
-------------------------------------------------------------------------------
data WhichPlayer
 = PlayerA
 | PlayerB


whichPlayerDecEq : (x , y : WhichPlayer) -> Dec (x = y)
%runElab deriveDecEq `{whichPlayerDecEq}

DecEq WhichPlayer where
 decEq x y = whichPlayerDecEq x y

data CardId = MkCardId WhichPlayer (Fin 25)

cardIdDecEq : (x, y : CardId) -> Dec (x = y)
%runElab deriveDecEq `{cardIdDecEq}

DecEq CardId where
 decEq x y = cardIdDecEq x y
-------------------------------------------------------------------------------
getOpponent : WhichPlayer -> WhichPlayer
getOpponent PlayerA = PlayerB
getOpponent PlayerB = PlayerA
-------------------------------------------------------------------------------
absoluteLowerBound : Integer
absoluteLowerBound = -1000
-------------------------------------------------------------------------------
absoluteUpperBound : Integer
absoluteUpperBound = 1000
-------------------------------------------------------------------------------
standardBounds : Type
standardBounds =
 Bounded
  Preliminaries.absoluteLowerBound
  Preliminaries.absoluteUpperBound
-------------------------------------------------------------------------------
maxSchool : Bounded 0 9
maxSchool = bind 9
-------------------------------------------------------------------------------
maxSchools : Vect 6 (Bounded 0 9)
maxSchools = replicate 6 maxSchool
-------------------------------------------------------------------------------
temporaryPermanentBase : Type -> Type
temporaryPermanentBase t = (t,t,t)
-------------------------------------------------------------------------------
extractBounded : Bounded lower upper -> Integer
extractBounded (MkBounded (n ** _)) = n
-------------------------------------------------------------------------------
sumVect : Vect n (Bounded lower upper) -> Integer
sumVect [] = 0
sumVect (x::xs) = (extractBounded x) + (sumVect xs)
-------------------------------------------------------------------------------
dominatesVect :
 Vect n (Bounded lower upper) ->
 Vect n (Bounded lower upper) ->
 Bool

dominatesVect [] [] = True
dominatesVect (x::xs) (y::ys) =
 (extractBounded x) >= (extractBounded y) && dominatesVect xs ys
-------------------------------------------------------------------------------
totalDifferenceVect :
 Vect n (Bounded lower upper) ->
 Vect n (Bounded lower upper) ->
 Integer

totalDifferenceVect [] [] = 0
totalDifferenceVect (x::xs) (y::ys) =
 ((extractBounded x) - (extractBounded y)) + (totalDifferenceVect xs ys)
-------------------------------------------------------------------------------
data MultiTree k = Node (MultiTree k) k Nat (MultiTree k) | Leaf
-------------------------------------------------------------------------------
insert : Ord k => MultiTree k -> k -> MultiTree k

insert Leaf key = Node Leaf key 1 Leaf
insert (Node left _key value right) key with (compare _key key)
  | EQ = Node left _key (value + 1) right
  | LT = Node (insert left key) _key value right
  | GT = Node left _key value (insert right key)
-------------------------------------------------------------------------------
get : Ord k => MultiTree k -> k -> Nat

get Left key = 0
get (Node left _key value right) key with (compare _key key)
 | EQ = value
 | LT = get left key
 | GT = get right key
-------------------------------------------------------------------------------
-- Ө(n log n)
dominates : Ord k => MultiTree k -> MultiTree k -> Bool

dominates _ Leaf = True
dominates bigger (Node left key value right) =
 ((get bigger key) >= value) &&
 (dominates bigger left) &&
 (dominates bigger right)
-------------------------------------------------------------------------------
{-not tail recursive-}
removeAt : List a -> Nat -> Maybe (List a)
removeAt [] _ = Nothing
removeAt (x::xs) Z = Just xs
removeAt (x::xs) (S k) with (removeAt xs k)
 | Nothing = Nothing
 | Just xs' = Just (x :: xs')
-------------------------------------------------------------------------------
replaceAt : Vect n a -> Fin n -> a -> Vect n a
replaceAt (x::xs) FZ y = (y::xs)
replaceAt (x::xs) (FS k) y = x :: (replaceAt xs k y)
-------------------------------------------------------------------------------


-- This uses an O(n^2) time algorithm for determining the uniqueness of lists.
-- Do not use for extremely large lists, etc.
-- The advantage of this algorithm is that the code and proofs are very simple,
-- and are also polymorphic without reference to any ordering, which is required by sorting algorithms.




{-
-- TODO: CHANGE NAT TO ARBITRARY INSTANCE OF DECEQ
uniqueList : List CardId -> Bool
uniqueList [] = True
uniqueList (x::xs) with (Prelude.List.find (==x) xs)
  | Nothing = uniqueList xs
  | Just _ = False
  

data UniqueList : List CardId -> Type where
  UniqueEmpty : UniqueList []
  UniqueConcat : UniqueList xs -> Prelude.List.find (==x) xs = Nothing -> UniqueList (x::xs)

notUniqueHead : (x : CardId) -> (xs : List CardId) -> (find (==x) xs = Nothing -> Void) -> UniqueList (x::xs) -> Void
notUniqueHead x xs inTail UniqueEmpty impossible
notUniqueHead x xs inTail (UniqueConcat uniqueTail notInTail) = inTail notInTail

notUniqueTail : (x : CardId) -> (xs : List CardId) -> (UniqueList xs -> Void) -> UniqueList (x::xs) -> Void
notUniqueTail x xs tailNotUnique UniqueEmpty impossible
notUniqueTail x xs tailNotUnique (UniqueConcat uniqueTail notInTail) = tailNotUnique uniqueTail


decUniqueList : (l : List CardId) -> Dec (UniqueList l)
decUniqueList [] = Yes UniqueEmpty
decUniqueList (x::xs) with (decEq (find (==x) xs) Nothing)
 | Yes headNotInTail with (decUniqueList xs)
   | Yes uniqueTail = Yes (UniqueConcat uniqueTail headNotInTail)
   | No tailNotUnique = No (notUniqueTail x xs tailNotUnique)
 | No headInTail = No (notUniqueHead x xs headInTail)
 -}
