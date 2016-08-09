module bounded_then_integer

import Data.So
import bounded

public export
(<) : Bounded lower upper -> Integer -> Bool
(<) (MkBounded (x1 ** _)) x2 = x1 < x2
public export
(>) : Bounded lower upper -> Integer -> Bool
(>) (MkBounded (x1 ** _)) x2 = x1 > x2
public export
(<=) : Bounded lower upper -> Integer -> Bool
(<=) (MkBounded (x1 ** _)) x2 = x1 <= x2
public export
(>=) : Bounded lower upper -> Integer -> Bool
(>=) (MkBounded (x1 ** _)) x2 = x1 >= x2
{-infixr 6 ===;-}
public export
(==) : Bounded lower upper -> Integer -> Bool
(==) (MkBounded (x1 ** _)) x2 = (x1 == x2)
public export
(+) : Bounded lower upper -> Integer -> Bounded lower upper
(+) x1 x2 = transformBounded (\x1 => x1 + x2) x1
public export
(-) : Bounded lower upper -> Integer -> Bounded lower upper
(-) x1 x2 = transformBounded (\x1 => x1 - x2) x1
public export
(*) : Bounded lower upper -> Integer -> Bounded lower upper
(*) x1 x2 = transformBounded (\x1 => x1 * x2) x1
{-
mod : Bounded lower upper -> Nat -> Bounded lower upper
mod x1 x2 = transformBounded (\x1 => mod x1 x2) x1

having trouble with integer mod nat currently.
-}
public export
div : Bounded lower upper -> Integer -> Bounded lower upper
div x1 x2 = transformBounded (\x1 => div x1 x2) x1
