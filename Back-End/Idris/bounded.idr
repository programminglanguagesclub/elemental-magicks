module Bounded

import Data.Vect
import Data.Fin
import Data.So

public export
data Bounded : Integer -> Integer -> Type where
  MkBounded : (n ** (So(lower <= n),So(n <= upper),So(lower <= upper))) -> Bounded lower upper

syntax ">>" [value] "<<" = MkBounded (value ** (Oh,Oh,Oh))

foo12 : Bounded 0 31
foo12 = >> 21 <<

{-not going to make Num instances yet for now-}
public export
Eq (Bounded lower upper) where
  (MkBounded(n1 ** _)) == (MkBounded(n2 ** _)) = (n1 == n2)
public export
Ord (Bounded lower upper) where
  compare (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = compare n1 n2
  (MkBounded(n1 ** _)) < (MkBounded(n2 ** _)) = n1 < n2
  (MkBounded(n1 ** _)) <= (MkBounded(n2 ** _)) = n1 <= n2
  (MkBounded(n1 ** _)) > (MkBounded(n2 ** _)) = n1 > n2
  (MkBounded(n1 ** _)) >= (MkBounded(n2 ** _)) = n1 >= n2
  {-max (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = max n1 n2{-max not done: shouldn't return Integer-}-} {-also do min-}
{-these only work if lower and upper are the same for each argument, otherwise use the functions below:-}

public export
lt : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
lt (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 < n2
public export
leq : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
leq (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 <= n2
public export
gt : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
gt (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 > n2
public export
geq : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
geq (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 >= n2
public export
eq : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
eq (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 == n2

public export
transformBounded : (Integer -> Integer) -> Bounded a b -> Bounded a b
transformBounded {a = lower} {b = upper}  f (MkBounded (x ** (proofLower,proofUpper,proofInhabitedInterval))) =
  let m = f x in
   case (choose (m <= upper)) of
    Left proofUpperBounded =>
     case (choose (lower <= m)) of
      Left proofLowerBounded =>
       MkBounded (m ** (proofLowerBounded,proofUpperBounded,proofInhabitedInterval))
      Right _ =>
       case (choose (lower <= lower)) of
       Left top => MkBounded (lower ** (top,proofInhabitedInterval,proofInhabitedInterval))
       Right bot => MkBounded (x ** (proofLower,proofUpper,proofInhabitedInterval)) {-this is an impossible case...-}   {-(lower ** (absurd bot,proofInhabitedInterval,proofInhabitedInterval))-}
    Right _ =>
     case (choose (upper <= upper)) of
     Left top =>  MkBounded (upper ** (proofInhabitedInterval,top,proofInhabitedInterval))
     Right bot => MkBounded (x ** (proofLower,proofUpper,proofInhabitedInterval)) {-again, impossible case-}
{-
max : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Integer
max (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = max n1 n2
min : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Integer
min (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = min n1 n2
-}

foo : Bounded 1 9
foo = MkBounded (3 ** (Oh,Oh,Oh))

bar : Bounded 1 9
bar = transformBounded (\x => x) foo

