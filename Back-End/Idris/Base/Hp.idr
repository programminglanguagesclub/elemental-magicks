module Base.Hp
import Data.So
import Base.Utility
import Base.Bounded
import Base.Preliminaries
%access public export
%default total

{-if I made this a GADT with a single constructor, then I can't pattern match on it apparently. This seems like a bug, although it doesn't matter-}

-------------------------------------------------------------------------------
data Hp = MkHp ((currentHp:Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound**(maxHp:Bounded 0 Preliminaries.absoluteUpperBound**So(leq currentHp maxHp))),{-baseHp:-}Bounded 1 Preliminaries.absoluteUpperBound)
-------------------------------------------------------------------------------
--syntax mkHp [hp] = MkHp (( >> hp << **( >> hp << ** Oh)), >> hp << )
-------------------------------------------------------------------------------
getCurrentHp : Hp -> Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound
getCurrentHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = currentHp
-------------------------------------------------------------------------------
getMaxHp : Hp -> Bounded 0 Preliminaries.absoluteUpperBound
getMaxHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = maxHp
-------------------------------------------------------------------------------
getBaseHp : Hp -> Bounded 1 Preliminaries.absoluteUpperBound
getBaseHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = baseHp
-------------------------------------------------------------------------------
maxHpLEQmaxHp : (maxHp : Bounded 0 Preliminaries.absoluteUpperBound) -> So(leq (the (Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound) (extendLowerBound maxHp Oh)) maxhp)
maxHpLEQmaxHp maxHp = believe_me Oh {-assumes that Preliminaries.absoluteLowerBound will never be strictly positive.-}
-------------------------------------------------------------------------------
generateHp : Bounded 1 Preliminaries.absoluteUpperBound -> Hp
generateHp baseHp = let maxHp = the (Bounded 0 Preliminaries.absoluteUpperBound) (extendLowerBound baseHp Oh) in
                        MkHp (((extendLowerBound maxHp Oh) ** (maxHp ** (maxHpLEQmaxHp maxHp))), baseHp)
-------------------------------------------------------------------------------
transformHp : (Integer -> Integer) -> Hp -> Hp
transformHp f (MkHp ((currentHp**(maxHp**prf)),baseHp)) =
 let m = transformBounded f currentHp in
     case (choose (leq m maxHp)) of
          Left proofUpperBound => MkHp ((m ** (maxHp ** proofUpperBound)),baseHp)
          Right _ => MkHp (((extendLowerBound maxHp Oh) ** (maxHp ** (maxHpLEQmaxHp maxHp))),baseHp)
transformMaxHp : (Integer -> Integer) -> Hp -> Hp
transformMaxHp f (MkHp ((currentHp**(maxHp**prf)),baseHp)) =
  let m = transformBounded f maxHp in
      case (choose (leq currentHp m)) of
           Left proofUpperBound => MkHp ((currentHp ** (m ** proofUpperBound)),baseHp)
           Right _ => MkHp (((extendLowerBound maxHp Oh) ** (maxHp ** (maxHpLEQmaxHp maxHp))),baseHp)
           {-the above two functions have some commonalities that can be pulled out (especially Right)-}

{-I feel I should be doing something monadic here, but it's perhaps a bit more complicated-}

rest : Hp -> Hp
rest (MkHp((currentHp**(maxHp**prf)),baseHp)) = transformHp (\x => (div4 $ extractBounded maxHp) + x) (MkHp((currentHp**(maxHp**prf)),baseHp))

--pierce : Integer {-should this take an integer or a bounded?-} -> Hp -> Hp
--pierce val (MkHp((currentHp**(maxHp**prf)),baseHp)) = transformHp (\x => x - val) (MkHp((currentHp**(maxHp**prf)),baseHp))

{-
namespace hp_then_integer
| -----------------------------------------------------------------------------
| lt :
|  Hp ->
|  Integer ->
|  Bool
|
| lt 
|
|
|
| 
-}
{-
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
-}

{-
(<) : Bounded lower upper -> Integer -> Bool
(<) (MkBounded (x1 ** _)) x2 = x1 < x2
(>) : Bounded lower upper -> Integer -> Bool
(>) (MkBounded (x1 ** _)) x2 = x1 > x2
(<=) : Bounded lower upper -> Integer -> Bool
(<=) (MkBounded (x1 ** _)) x2 = x1 <= x2
(>=) : Bounded lower upper -> Integer -> Bool
(>=) (MkBounded (x1 ** _)) x2 = x1 >= x2
{-infixr 6 ===;-}
(==) : Bounded lower upper -> Integer -> Bool
(==) (MkBounded (x1 ** _)) x2 = (x1 == x2)
(+) : Bounded lower upper -> Integer -> Bounded lower upper
(+) x1 x2 = transformBounded (\x1 => x1 + x2) x1
(-) : Bounded lower upper -> Integer -> Bounded lower upper
(-) x1 x2 = transformBounded (\x1 => x1 - x2) x1
(*) : Bounded lower upper -> Integer -> Bounded lower upper
(*) x1 x2 = transformBounded (\x1 => x1 * x2) x1
infixr 4 :=
(:=) : Bounded lower upper -> Integer -> Bounded lower upper
(:=) x1 x2 = transformBounded (\x1 => x2) x1

{-
mod : Bounded lower upper -> Nat -> Bounded lower upper
mod x1 x2 = transformBounded (\x1 => mod x1 x2) x1
having trouble with integer mod nat currently.
-}
partial
div : Bounded lower upper -> Integer -> Bounded lower upper
div x1 x2 = transformBounded (\x1 => div x1 x2) x1
-}

{-
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
partial
div : Integer -> Bounded lower upper -> Bounded lower upper
div x1 x2 = transformBounded (\x2 => div x1 x2) x2


-}




