module hp

import Data.So
import Data.Primitives.Views
import bounded
import bounded_then_integer
import preliminaries
%access public export
%default total
{-if I made this a GADT with a single constructor, then I can't pattern match on it apparently. This seems like a bug, although it doesn't matter-}
data Hp = MkHp ((currentHp:Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound**(maxHp:Bounded 0 Preliminaries.absoluteUpperBound**So(leq currentHp maxHp))),{-baseHp:-}Bounded 1 Preliminaries.absoluteUpperBound)

syntax mkHp [hp] = MkHp (( >> hp << **( >> hp << ** Oh)), >> hp << )

getCurrentHp : Hp -> Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound
getCurrentHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = currentHp
getMaxHp : Hp -> Bounded 0 Preliminaries.absoluteUpperBound
getMaxHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = maxHp
getBaseHp : Hp -> Bounded 1 Preliminaries.absoluteUpperBound
getBaseHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = baseHp
transformHp : (Integer -> Integer) -> Hp -> Hp
transformHp f (MkHp ((currentHp**(maxHp**prf)),baseHp)) = ?hole
{- let m = f x in
   case (choose (m <= maxHp)) of
    Left proofUpperBounded =>
     case (choose ( <= m)) of
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
     -}

transformMaxHp : (Integer -> Integer) -> Hp -> Hp
transformMaxHp f (MkHp ((currentHp**(maxHp**prf)),baseHp)) = ?hole

{-I feel I should be doing something monadic here, but it's perhaps a bit more complicated-}

rest : Hp -> Hp
rest (MkHp((currentHp**(maxHp**prf)),baseHp)) = transformHp (\x => (div4 $ extractBounded maxHp) + x) (MkHp((currentHp**(maxHp**prf)),baseHp))

pierce : Integer {-should this take an integer or a bounded?-} -> Hp -> Hp
pierce val (MkHp((currentHp**(maxHp**prf)),baseHp)) = transformHp (\x => x - val) (MkHp((currentHp**(maxHp**prf)),baseHp))
















