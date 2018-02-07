module Base.Bounded
import Data.Vect
import Data.So

%access public export
%default total

-------------------------------------------------------------------------------
data Bounded : Integer -> Integer -> Type where
  MkBounded : (n ** (So(lower <= n),So(n <= upper))) -> Bounded lower upper
-------------------------------------------------------------------------------
bind :
 (n : Integer) ->
 {lower : Integer} ->
 {upper : Integer} ->
 {auto boundedBelow : So (lower <= n)} ->
 {auto boundedAbove : So (n <= upper)} ->
 Bounded lower upper

bind n {boundedBelow=boundedBelow} {boundedAbove=boundedAbove} =
 MkBounded (n ** (boundedBelow,boundedAbove))
-------------------------------------------------------------------------------
my_lte_transitive : {a,b,c : Integer} -> So(a<=b) -> So(b<=c) -> So(a<=c)
my_lte_transitive _ _ = believe_me Oh
-------------------------------------------------------------------------------
my_lte_reflexive : (a : Integer) -> So(a<=a)
my_lte_reflexive a = believe_me Oh
-------------------------------------------------------------------------------
integerToBounded : -- MAKE USE DEC!!
 (n : Integer) ->
 (lower : Integer) ->
 (upper : Integer) ->
 Maybe (Bounded lower upper)

integerToBounded m lower upper =
 case (choose (m <= upper)) of
  Left proofUpperBounded =>
   case (choose (lower <= m)) of
    Left proofLowerBounded =>
     Just $ MkBounded (m ** (proofLowerBounded,proofUpperBounded))
    Right _ => Nothing
  Right _ => Nothing
-------------------------------------------------------------------------------
Eq (Bounded lower upper) where
  (MkBounded(n1 ** _)) == (MkBounded(n2 ** _)) = (n1 == n2)
-------------------------------------------------------------------------------
Ord (Bounded lower upper) where
  compare (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = compare n1 n2
  (MkBounded(n1 ** _)) < (MkBounded(n2 ** _)) = n1 < n2
  (MkBounded(n1 ** _)) <= (MkBounded(n2 ** _)) = n1 <= n2
  (MkBounded(n1 ** _)) > (MkBounded(n2 ** _)) = n1 > n2
  (MkBounded(n1 ** _)) >= (MkBounded(n2 ** _)) = n1 >= n2
  {-max (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = max n1 n2{-max not done: shouldn't return Integer-}-} {-also do min-}
{-these only work if lower and upper are the same for each argument, otherwise use the functions below:-}
-------------------------------------------------------------------------------
lt : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
lt (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 < n2
-------------------------------------------------------------------------------
leq : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
leq (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 <= n2
-------------------------------------------------------------------------------
gt : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
gt (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 > n2
-------------------------------------------------------------------------------
geq : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
geq (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 >= n2
-------------------------------------------------------------------------------
eq : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Bool
eq (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = n1 == n2
-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------
max : Bounded lower upper -> Bounded lower upper -> Integer
max (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = max n1 n2
-------------------------------------------------------------------------------
min : Bounded lower1 upper1 -> Bounded lower2 upper2 -> Integer
min (MkBounded(n1 ** _)) (MkBounded(n2 ** _)) = min n1 n2
-------------------------------------------------------------------------------
extendLowerBound : Bounded a b -> So(a' <= a) -> Bounded a' b
extendLowerBound (MkBounded (n ** (prf_lower_n, prf_n_upper))) prf_extend =
 MkBounded (n ** ((my_lte_transitive prf_extend prf_lower_n),prf_n_upper))
-------------------------------------------------------------------------------
extendUpperBound : Bounded a b -> So(b <= b') -> Bounded a b'
extendUpperBound (MkBounded (n ** (prf_lower_n, prf_n_upper))) prf_extend =
 MkBounded (n ** (prf_lower_n,(my_lte_transitive prf_n_upper prf_extend)))
-------------------------------------------------------------------------------
extendBounds : Bounded a b -> {auto prf_extend_lower : So(a' <= a)} -> {auto prf_extend_upper : So(b <= b')} -> Bounded a' b'
extendBounds {prf_extend_lower = prf_extend_lower}{prf_extend_upper = prf_extend_upper}(MkBounded (n ** (prf_lower_n, prf_n_upper))) =
 MkBounded (n ** ((my_lte_transitive prf_extend_lower prf_lower_n),(my_lte_transitive prf_n_upper prf_extend_upper)))
-------------------------------------------------------------------------------
removeUpperBound : Bounded 0 b -> Nat
removeUpperBound (MkBounded (n ** (_, prf_n_upper))) = {-with (compare n 0)-}
 fromIntegerNat n -- this is not very type correct, but it works.
-------------------------------------------------------------------------------
removeBounds : Bounded a b -> Integer
removeBounds (MkBounded (n ** _)) = n
-------------------------------------------------------------------------------
namespace Integer_then_bounded
-------------------------------------------------------------------------------
 (<) : Integer -> Bounded lower upper -> Bool
 (<) x1 (MkBounded (x2 ** _)) = x1 < x2
-------------------------------------------------------------------------------
 (>) : Integer -> Bounded lower upper -> Bool
 (>) x1 (MkBounded (x2 ** _)) = x1 > x2
-------------------------------------------------------------------------------
 (<=) : Integer -> Bounded lower upper -> Bool
 (<=) x1 (MkBounded (x2 ** _)) = x1 <= x2
-------------------------------------------------------------------------------
 (>=) : Integer -> Bounded lower upper -> Bool
 (>=) x1 (MkBounded (x2 ** _)) = x1 >= x2
-------------------------------------------------------------------------------
 (==) : Integer -> Bounded lower upper -> Bool
 (==) x1 (MkBounded (x2 ** _)) = (x1 == x2)
-------------------------------------------------------------------------------
 (+) : Integer -> Bounded lower upper -> Bounded lower upper
 (+) x1 x2 = transformBounded (\x2 => x1 + x2) x2
-------------------------------------------------------------------------------
 (-) : Integer -> Bounded lower upper -> Bounded lower upper
 (-) x1 x2 = transformBounded (\x2 => x1 - x2) x2
-------------------------------------------------------------------------------
 (*) : Integer -> Bounded lower upper -> Bounded lower upper
 (*) x1 x2 = transformBounded (\x2 => x1 * x2) x2
-------------------------------------------------------------------------------
 {-
 mod : Nat -> Bounded lower upper -> Bounded lower upper
 mod x1 x2 = transformBounded (\x2 => mod x1 x2) x2
 -}
 partial
 div : Integer -> Bounded lower upper -> Bounded lower upper
 div x1 x2 = transformBounded (\x2 => div x1 x2) x2
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
namespace Bounded_then_integer
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
 (<) : Bounded lower upper -> Integer -> Bool
 (<) (MkBounded (x1 ** _)) x2 = x1 < x2
-------------------------------------------------------------------------------
 (>) : Bounded lower upper -> Integer -> Bool
 (>) (MkBounded (x1 ** _)) x2 = x1 > x2
-------------------------------------------------------------------------------
 (<=) : Bounded lower upper -> Integer -> Bool
 (<=) (MkBounded (x1 ** _)) x2 = x1 <= x2
-------------------------------------------------------------------------------
 (>=) : Bounded lower upper -> Integer -> Bool
 (>=) (MkBounded (x1 ** _)) x2 = x1 >= x2
-------------------------------------------------------------------------------
 (==) : Bounded lower upper -> Integer -> Bool
 (==) (MkBounded (x1 ** _)) x2 = (x1 == x2)
-------------------------------------------------------------------------------
 (+) : Bounded lower upper -> Integer -> Bounded lower upper
 (+) x1 x2 = transformBounded (\x1 => x1 + x2) x1
-------------------------------------------------------------------------------
 (-) : Bounded lower upper -> Integer -> Bounded lower upper
 (-) x1 x2 = transformBounded (\x1 => x1 - x2) x1
-------------------------------------------------------------------------------
 (*) : Bounded lower upper -> Integer -> Bounded lower upper
 (*) x1 x2 = transformBounded (\x1 => x1 * x2) x1
-------------------------------------------------------------------------------
 infixr 4 :=
 (:=) : Bounded lower upper -> Integer -> Bounded lower upper
 (:=) x1 x2 = transformBounded (\x1 => x2) x1
-------------------------------------------------------------------------------
 {-
 mod : Bounded lower upper -> Nat -> Bounded lower upper
 mod x1 x2 = transformBounded (\x1 => mod x1 x2) x1
 having trouble with integer mod nat currently.
 -}
 partial
 div : Bounded lower upper -> Integer -> Bounded lower upper
 div x1 x2 = transformBounded (\x1 => div x1 x2) x1
-------------------------------------------------------------------------------




