module Base.Bounded

import Data.Vect
import Data.Fin
import Data.So
import public Base.Utility
%access public export
%default total

data Bounded : Integer -> Integer -> Type where
  MkBounded : (n ** (So(lower <= n),So(n <= upper))) -> Bounded lower upper

syntax ">>" [value] "<<" = MkBounded (value ** (Oh,Oh))

foo12 : Bounded 0 31
foo12 = >> 21 <<



my_lte_transitive : {a,b,c : Integer} -> So(a<=b) -> So(b<=c) -> So(a<=c)
my_lte_transitive _ _ = believe_me Oh

my_lte_reflexive : (a : Integer) -> So(a<=a)
my_lte_reflexive a = believe_me Oh

integerToBounded : Integer -> (lower : Integer) -> (upper : Integer) -> Maybe (Bounded lower upper)
integerToBounded m lower upper = case (choose (m <= upper)) of
                                      Left proofUpperBounded => case (choose (lower <= m)) of
                                                                     Left proofLowerBounded => Just $ MkBounded (m ** (proofLowerBounded,proofUpperBounded))
                                                                     Right _ => Nothing
                                      Right _ => Nothing



{-not going to make Num instances yet for now-}

Eq (Bounded lower upper) where
  (MkBounded(n1 ** _)) == (MkBounded(n2 ** _)) = (n1 == n2)
Ord (Bounded lower upper) where
  compare (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = compare n1 n2
  (MkBounded(n1 ** _)) < (MkBounded(n2 ** _)) = n1 < n2
  (MkBounded(n1 ** _)) <= (MkBounded(n2 ** _)) = n1 <= n2
  (MkBounded(n1 ** _)) > (MkBounded(n2 ** _)) = n1 > n2
  (MkBounded(n1 ** _)) >= (MkBounded(n2 ** _)) = n1 >= n2
  {-max (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = max n1 n2{-max not done: shouldn't return Integer-}-} {-also do min-}
{-these only work if lower and upper are the same for each argument, otherwise use the functions below:-}

lt : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
lt (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 < n2
leq : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
leq (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 <= n2
gt : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
gt (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 > n2
geq : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
geq (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 >= n2
eq : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
eq (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 == n2

transformBounded : (Integer -> Integer) -> Bounded a b -> Bounded a b
transformBounded {a = lower} {b = upper}  f (MkBounded (x ** (proofLower,proofUpper))) =
  let m = f x in
   case (choose (m <= upper)) of
    Left proofUpperBounded =>
     case (choose (lower <= m)) of
      Left proofLowerBounded =>
       MkBounded (m ** (proofLowerBounded,proofUpperBounded))
      Right _ => MkBounded (lower ** (my_lte_reflexive lower,my_lte_transitive proofLower proofUpper))
    Right _ => MkBounded (upper ** (my_lte_transitive proofLower proofUpper,my_lte_reflexive upper))

max : Bounded lower upper -> Bounded lower upper -> Integer
max (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = max n1 n2
min : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Integer
min (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = min n1 n2

foo : Bounded 1 9
foo = MkBounded (3 ** (Oh,Oh))

bar : Bounded 1 9
bar = transformBounded (\x => x) foo

{-
postulate extendOkay : {a,b,c : Integer} -> So(a <= b) -> So(b <= c) -> So(a <= c)

I don't know if I like this because I want to be able to pattern match with Oh probably.....

I'm not sure this does that.

-}


extendLowerBound : Bounded a b -> So(a' <= a) -> Bounded a' b
extendLowerBound (MkBounded (n ** (prf_lower_n, prf_n_upper))) prf_extend = MkBounded (n ** ((my_lte_transitive prf_extend prf_lower_n),prf_n_upper))

extendUpperBound : Bounded a b -> So(b <= b') -> Bounded a b'
extendUpperBound (MkBounded (n ** (prf_lower_n, prf_n_upper))) prf_extend = MkBounded (n ** (prf_lower_n,(my_lte_transitive prf_n_upper prf_extend)))


extendBounds : Bounded a b -> So(a' <= a) -> So(b <= b') -> Bounded a' b'
extendBounds (MkBounded (n ** (prf_lower_n, prf_n_upper))) prf_extend_lower prf_extend_upper = MkBounded (n ** ((my_lte_transitive prf_extend_lower prf_lower_n),(my_lte_transitive prf_n_upper prf_extend_upper)))







