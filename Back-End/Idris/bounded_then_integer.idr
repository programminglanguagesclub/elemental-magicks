module bounded_then_integer

import Data.So
import bounded

(<) : Bounded (lower,upper) -> Integer -> Bool
(<) (x1 ** _) x2 = x1 < x2
(>) : Bounded (lower,upper) -> Integer -> Bool
(>) (x1 ** _) x2 = x1 > x2
(<=) : Bounded (lower,upper) -> Integer -> Bool
(<=) (x1 ** _) x2 = x1 <= x2
(>=) : Bounded (lower,upper) -> Integer -> Bool
(>=) (x1 ** _) x2 = x1 >= x2
{-infixr 6 ===;-}
(==) : Bounded (lower,upper) -> Integer -> Bool
(==) (x1 ** _) x2 = (x1 == x2)

(+) : Bounded (lower,upper) -> Integer -> Bounded (lower,upper)
(+) x1 x2 = transformBounded (\x1 => x1 + x2) x1
(-) : Bounded (lower,upper) -> Integer -> Bounded (lower,upper)
(-) x1 x2 = transformBounded (\x1 => x1 - x2) x1
(*) : Bounded (lower,upper) -> Integer -> Bounded (lower,upper)
(*) x1 x2 = transformBounded (\x1 => x1 * x2) x1
{-
mod : Bounded (lower,upper) -> Nat -> Bounded (lower,upper)
mod x1 x2 = transformBounded (\x1 => mod x1 x2) x1

having trouble with integer mod nat currently.
-}
div : Bounded (lower,upper) -> Integer -> Bounded (lower,upper)
div x1 x2 = transformBounded (\x1 => div x1 x2) x1
