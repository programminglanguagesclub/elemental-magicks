module preliminaries

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

public export Level : Type
Level = Bounded 0 9 {- bounds for card level and schools -}
{- this should have a bound of 1 for base -}


public export Schools : Type
Schools = (Level,Level,Level,Level,Level,Level)


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

public export ExtractBounded : Bounded lower upper -> Integer
ExtractBounded (n ** _) = n

public export BoardIndex : Type
BoardIndex = Bounded 0 8