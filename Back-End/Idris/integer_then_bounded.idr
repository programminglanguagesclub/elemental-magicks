module integer_then_bounded

import Data.So
import bounded

{-bounded should probably be part of the Num and Integral typeclasses-}


(<) : Integer -> Bounded lower upper -> Bool
(<) x1 (MkBounded (x2 ** _)) = x1 < x2
(>) : Integer -> Bounded lower upper -> Bool
(>) x1 (MkBounded (x2 ** _)) = x1 > x2
(<=) : Integer -> Bounded lower upper -> Bool
(<=) x1 (MkBounded (x2 ** _)) = x1 <= x2
(>=) : Integer -> Bounded lower upper -> Bool
(>=) x1 (MkBounded (x2 ** _)) = x1 >= x2
{-infixr 6 ===;-}
(==) : Integer -> Bounded lower upper -> Bool
(==) x1 (MkBounded (x2 ** _)) = (x1 == x2)

(+) : Integer -> Bounded lower upper -> Bounded lower upper
(+) x1 x2 = transformBounded (\x2 => x1 + x2) x2
(-) : Integer -> Bounded lower upper -> Bounded lower upper
(-) x1 x2 = transformBounded (\x2 => x1 - x2) x2
(*) : Integer -> Bounded lower upper -> Bounded lower upper
(*) x1 x2 = transformBounded (\x2 => x1 * x2) x2
{-
mod : Nat -> Bounded lower upper -> Bounded lower upper
mod x1 x2 = transformBounded (\x2 => mod x1 x2) x2
-}

div : Integer -> Bounded lower upper -> Bounded lower upper
div x1 x2 = transformBounded (\x2 => div x1 x2) x2
