module Preliminaries

import Data.Vect
import Data.Fin
import Data.So

public export Bounded : Integer -> Integer -> Type
Bounded lower upper = (n ** So (n >= lower && n <= upper))
public export absoluteLowerBound : Integer
absoluteLowerBound = -1000
public export absoluteUpperBound : Integer
absoluteUpperBound = 1000
public export Range : Type
Range = Bounded 0 5
public export Speed : Type
Speed = Bounded absoluteLowerBound absoluteUpperBound
public export Defense : Type
Defense = Bounded 0 absoluteUpperBound
public export Attack : Type
Attack = Bounded 0 absoluteUpperBound


public export Hp : {- should probably just use data. Might be better to use data for the others too, because then I won't be able to use attack in place of defense, etc. -}


{- mess
public export
Hp : (baseHp : Bounded 1 absoluteUpperBound) -> Type
Hp baseHp = {currentHp : Bounded absoluteLowerBound absoluteUpperBound} -> {maxHp : Bounded 0 absoluteUpperBound} -> ((currentHp,maxHp) ** So(currentHp = baseHp))
-}

public export Level : Type
Level = Bounded 0 9 {- bounds for card level and schools -}
{- this should have a bound of 1 for base -}

public export
School : Type
{-School = Bounded 0 5-}
School = Fin 6
public export Schools : Type
Schools = Vect 6 Level

public export
maxSchool : Level
maxSchool = (9 ** Oh)
public export
maxSchools : Schools
maxSchools = [maxSchool,maxSchool,maxSchool,maxSchool,maxSchool,maxSchool]

public export
TemporaryPermanentBase : Type -> Type
TemporaryPermanentBase t = (t,t,t)
public export
getTemporary : (t,t',t'') -> t
getTemporary (temporary,_,_) = temporary
public export
getPermanent : (t,t',t'') -> t'
getPermanent (_,permanent,_) = permanent
public export
getBase : (t,t',t'') -> t''
getBase (_,_,base) = base


{- remove these -}
public export
getTemporaryLevel : (t,t,t'') -> t
getTemporaryLevel (temporaryLevel,_,_) = temporaryLevel
public export
getPermanentLevel : (t,t,t'') -> t
getPermanentLevel (_,permanentLevel,_) = permanentLevel
public export
getBaseLevel : (t,t,t'') -> t''
getBaseLevel (_,_,baseLevel) = baseLevel
{-/remove these-}


{-actually this one I might want to hide...-}
public export transformBounded : (lower:Integer) -> (upper:Integer) -> So (lower >= lower && lower <= upper) -> So (upper >= lower && upper <= upper) -> (Integer -> Integer) -> Bounded lower upper -> Bounded lower upper
transformBounded lower upper ProofLower ProofUpper f (n ** _) =
 let m = f n in
  case (choose (m <= upper)) of
   Left ProofUpperBounded =>
    case (choose (m >= lower && (m <= upper))) of
     Left ProofBounded =>
      (m ** ProofBounded)
     Right _ =>
      (lower ** ProofLower) {- must make sure that lower is less than upper -}
   Right _ =>
    (upper ** ProofUpper)

{-
public export
increment : (lower:Integer) -> (upper:Integer) -> So (lower >= lower && lower <= upper) -> So (upper >= lower && upper <= upper) -> Integer -> Bounded lower upper -> Bounded lower upper
increment lower upper ProofLower ProofUpper v proof = transformBounded lower upper ProofLower ProofUpper (\x => x+v) proof
-}





public export transformRange : (Integer -> Integer) -> Range -> Range
transformRange = transformBounded 0 5 Oh Oh
public export transformSpeed : (Integer -> Integer) -> Speed -> Speed
transformSpeed = transformBounded absoluteLowerBound absoluteUpperBound Oh Oh
public export transformLevel : (Integer -> Integer) -> Level -> Level
transformLevel = transformBounded 0 9 Oh Oh
public export transformAttack : (Integer -> Integer) -> Attack -> Attack
transformAttack = transformBounded 0 absoluteUpperBound Oh Oh

public export extractBounded : Bounded lower upper -> Integer
extractBounded (n ** _) = n

public export BoardIndex : Type
BoardIndex = Fin 9
public export HandIndex : Type
HandIndex = Fin 25
public export GraveyardIndex : Type
GraveyardIndex = Fin 25
{-these could maybe be better. I don't have anything at the type level that forces them to be valid hand or graveyard indices... I can fix this later-}

{- Hand and Graveyard indices point to the last available slot if they overshoot. The last available slot is the first one with no card in it. (so have to make sure there's not more than 1 that overshoots!)-}


public export
sumVect : Vect n (Bounded lower upper) -> Integer
sumVect [] = 0
sumVect (x::xs) = (extractBounded x) + (sumVect xs)

{-This can be made much more general when type classes are added to Bounded-}
public export
dominatesVect : Vect n (Bounded lower upper) -> Vect n (Bounded lower upper) -> Bool
dominatesVect [] [] = True
dominatesVect (x::xs) (y::ys) = (extractBounded x) >= (extractBounded y) && dominatesVect xs ys

public export
totalDifferenceVect : Vect n (Bounded lower upper) -> Vect n (Bounded lower upper) -> Integer
totalDifferenceVect [] [] = 0
totalDifferenceVect (x::xs) (y::ys) = ((extractBounded x) - (extractBounded y)) + (totalDifferenceVect xs ys)


public export
data MultiTree k = Node (MultiTree k) k Nat (MultiTree k) | Leaf
public export
insert : Ord k => MultiTree k -> k -> MultiTree k
insert Leaf key = Node Leaf key 1 Leaf
insert (Node left _key value right) key = if (_key == key)
                                           then Node left _key (value + 1) right
                                          else if (key < _key)
                                           then Node (insert left key) _key value right
                                           else Node left _key value (insert right key)

public export
get : Ord k => MultiTree k -> k -> Nat
get Left key = 0
get (Node left _key value right) key = if (_key == key)
                                        then value
                                       else if (key < _key)
                                        then (get left key)
                                        else (get right key)

{- Ө(n log n) -}
public export {-this should be made into a typeclass-}
dominates : Ord k => MultiTree k -> MultiTree k -> Bool
dominates _ Leaf = True
dominates bigger (Node left key value right) = ((get bigger key) >= value) && (dominates bigger left) && (dominates bigger right)





public export
extractBoundedNat : Bounded 0 upperBound -> Nat {-have to use the proof terms to reject the cases where the number is negative? Or could just project to Nat...-}



public export {-not tail recursive-}
removeAt : List a -> Nat -> Maybe (List a)
removeAt [] _ = Nothing
removeAt (x::xs) Z = Just xs
removeAt (x::xs) (S k) with (removeAt xs k)
 | Nothing = Nothing
 | Just xs' = Just (x :: xs')


public export
replaceAt : Vect n a -> Fin n -> a -> Vect n a
replaceAt (x::xs) FZ y = (y::xs)
replaceAt (x::xs) (FS k) y = x :: (replaceAt xs k y)


{-replaceAt : List a -> Nat -> a -> List a {-ignores out of bounds case-}
replaceAt [] _ _ = []
replaceAt (x::xs) Z y = (y::xs)
replaceAt (x::xs) (S k) y = x :: (replaceAt xs k y)
-}










