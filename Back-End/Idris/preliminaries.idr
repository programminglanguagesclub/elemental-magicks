module Preliminaries
import Data.Vect
import Data.Fin
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
%access public export
%default total
absoluteLowerBound : Integer
absoluteLowerBound = -1000
absoluteUpperBound : Integer
absoluteUpperBound = 1000

maxSchool : Bounded 0 9
maxSchool = >> 9 <<
maxSchools : Vect 6 (Bounded 0 9)
maxSchools = [maxSchool,maxSchool,maxSchool,maxSchool,maxSchool,maxSchool]
temporaryPermanentBase : Type -> Type
temporaryPermanentBase t = (t,t,t)

extractBounded : Bounded lower upper -> Integer
extractBounded (MkBounded (n ** _)) = n

{-these could maybe be better. I don't have anything at the type level that forces them to be valid hand or graveyard indices... I can fix this later-}

{- Hand and Graveyard indices point to the last available slot if they overshoot. The last available slot is the first one with no card in it. (so have to make sure there's not more than 1 that overshoots!)-}

sumVect : Vect n (Bounded lower upper) -> Integer
sumVect [] = 0
sumVect (x::xs) = (extractBounded x) + (sumVect xs)

{-This can be made much more general when type classes are added to Bounded-}
dominatesVect : Vect n (Bounded lower upper) -> Vect n (Bounded lower upper) -> Bool
dominatesVect [] [] = True
dominatesVect (x::xs) (y::ys) = (extractBounded x) >= (extractBounded y) && dominatesVect xs ys

totalDifferenceVect : Vect n (Bounded lower upper) -> Vect n (Bounded lower upper) -> Integer
totalDifferenceVect [] [] = 0
totalDifferenceVect (x::xs) (y::ys) = ((extractBounded x) - (extractBounded y)) + (totalDifferenceVect xs ys)

data MultiTree k = Node (MultiTree k) k Nat (MultiTree k) | Leaf
insert : Ord k => MultiTree k -> k -> MultiTree k
insert Leaf key = Node Leaf key 1 Leaf
insert (Node left _key value right) key = if (_key == key)
                                           then Node left _key (value + 1) right
                                          else if (key < _key)
                                           then Node (insert left key) _key value right
                                           else Node left _key value (insert right key)

get : Ord k => MultiTree k -> k -> Nat
get Left key = 0
get (Node left _key value right) key = if (_key == key)
                                        then value
                                       else if (key < _key)
                                        then (get left key)
                                        else (get right key)

{- Ө(n log n) -}
{-this should be made into a typeclass-}
dominates : Ord k => MultiTree k -> MultiTree k -> Bool
dominates _ Leaf = True
dominates bigger (Node left key value right) = ((get bigger key) >= value) && (dominates bigger left) && (dominates bigger right)




{-not tail recursive-}
removeAt : List a -> Nat -> Maybe (List a)
removeAt [] _ = Nothing
removeAt (x::xs) Z = Just xs
removeAt (x::xs) (S k) with (removeAt xs k)
 | Nothing = Nothing
 | Just xs' = Just (x :: xs')



replaceAt : Vect n a -> Fin n -> a -> Vect n a
replaceAt (x::xs) FZ y = (y::xs)
replaceAt (x::xs) (FS k) y = x :: (replaceAt xs k y)


