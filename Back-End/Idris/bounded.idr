module Bounded

import Data.Vect
import Data.Fin
import Data.So


public export
data Bounded : Integer -> Integer -> Type where
  MkBounded : (n ** (So(lower <= n),So(n <= upper),So(lower <= upper))) -> Bounded lower upper


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


foo : Bounded 1 9
foo = MkBounded (3 ** (Oh,Oh,Oh))

bar : Bounded 1 9
bar = transformBounded (\x => x) foo

