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

{- mess
public export
Hp : (baseHp : Bounded 1 absoluteUpperBound) -> Type
Hp baseHp = {currentHp : Bounded absoluteLowerBound absoluteUpperBound} -> {maxHp : Bounded 0 absoluteUpperBound} -> ((currentHp,maxHp) ** So(currentHp = baseHp))
-}

public export Level : Type
Level = Bounded 0 9 {- bounds for card level and schools -}
{- this should have a bound of 1 for base -}

School : Type
School = Bounded 0 5
public export Schools : Type
Schools = Vect 6 Level
public export TemporaryPermanentBase : Type -> Type
TemporaryPermanentBase t = (t,t,t)

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















