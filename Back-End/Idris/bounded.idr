module Bounded

import Data.Vect
import Data.Fin
import Data.So

public export
Bounded : Integer -> Integer -> Type
Bounded lower upper = (n ** (So(lower <= n), So(n <= upper), So(lower <= upper)))

public export
transformBounded : (Integer -> Integer) -> Bounded a b -> Bounded a b
transformBounded {a = lower} {b = upper}  f (x ** (proofLower,proofUpper,proofInhabitedInterval)) =
  let m = f x in
   case (choose (m <= upper)) of
    Left proofUpperBounded =>
     case (choose (lower <= m)) of
      Left proofLowerBounded =>
       (m ** (proofLowerBounded,proofUpperBounded,proofInhabitedInterval))
      Right _ =>
       case (choose (lower <= lower)) of
       Left top => (lower ** (top,proofInhabitedInterval,proofInhabitedInterval))
       Right bot => (x ** (proofLower,proofUpper,proofInhabitedInterval)) {-this is an impossible case...-}   {-(lower ** (absurd bot,proofInhabitedInterval,proofInhabitedInterval))-}
    Right _ =>
     case (choose (upper <= upper)) of
     Left top =>  (upper ** (proofInhabitedInterval,top,proofInhabitedInterval))
     Right bot => (x ** (proofLower,proofUpper,proofInhabitedInterval)) {-again, impossible case-}


foo : Bounded 1 9
foo = (3 ** (Oh,Oh,Oh))

bar : Bounded 1 9
bar = transformBounded (\x => x) foo

