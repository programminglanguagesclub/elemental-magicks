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


generateHp : Bounded 1 Preliminaries.absoluteUpperBound -> Hp
             
             
             
            {- ((currentHp:Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound**(maxHp:Bounded 0 Preliminaries.absoluteUpperBound**So(leq currentHp maxHp))),Bounded 1 Preliminaries.absoluteUpperBound)-}

syntax mkHp [hp] = MkHp (( >> hp << **( >> hp << ** Oh)), >> hp << )

testHp : Hp
testHp = mkHp 20




{-
eqLeq : (m : Bounded a b) -> So(leq m m)
eqLeq m = case (choose (leq m m)) of
               Left prfTrue => prfTrue
               Right prfFalse => rewrite my_lte_reflexive in (absurd prfFalse)
               -}
{-
my_eq_leq : (m : Bounded a b) -> So(leq m m)
my_eq_leq = believe_me Oh
-}
{-
eqLeq : (n : Bounded a b) -> (m : Bounded a' b') -> 
        -}
{-
eq_hp : (c : Integer) -> So(leq (the (Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound) (MkHp (c** (Oh,Oh)))) (the (Bounded 0 Preliminaries.absoluteUpperBound) (MkHp (c ** (Oh,Oh)))))
eq_hp c = believe_me Oh
-}
{-
eq_hp : So(m <= m) -> Bounded a b
-}


leq_hp_maxhp : (currentHp : Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound) -> (maxHp : Bounded 0 Preliminaries.absoluteUpperBound) -> So((extractBounded currentHp) <= (extractBounded maxHp)) -> So(leq currentHp maxHp)
leq_hp_maxhp currentHp maxHp _ = believe_me Oh


{-the above is very hard to use...-}


getCurrentHp : Hp -> Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound
getCurrentHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = currentHp
getMaxHp : Hp -> Bounded 0 Preliminaries.absoluteUpperBound
getMaxHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = maxHp
getBaseHp : Hp -> Bounded 1 Preliminaries.absoluteUpperBound
getBaseHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = baseHp

{-
leqProof : (i : Integer) -> leq (the (Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound) (MkBounded ( i ** _))) (the (Bounded 0 Preliminaries.absoluteUpperBound) (MkBounded ( i ** _))) = True
-}

{-
transformHp : (Integer -> Integer) -> Hp -> Hp
transformHp f (MkHp (((MkBounded (currentHp ** prfCurrentHpBounded)) ** ((MkBounded (maxHp ** prfMaxHpBounded)) ** prf)),baseHp)) =
  let m = transformBounded f (MkBounded (currentHp ** prfCurrentHpBounded)) in
      case (choose (leq m (MkBounded (maxHp ** prfMaxHpBounded)))) of
           Left proofUpperBound => MkHp ((m ** ((MkBounded (maxHp ** prfMaxHpBounded))) ** proofUpperBound), baseHp)
           Right _ => MkHp(((extendLowerBound (MkBounded (maxHp ** prfMaxHpBounded)) (rewrite leqProof maxHp in Oh)) ** ((MkBounded (maxHp ** prfMaxHpBounded))) ** Oh),baseHp)

-}


{-assumes that Preliminaries.absoluteLowerBound will never be strictly positive.-}
maxHpLEQmaxHp : (maxHp : Bounded 0 Preliminaries.absoluteUpperBound) -> So(leq (the (Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound) (extendLowerBound maxHp Oh)) maxhp)
maxHpLEQmaxHp maxHp = believe_me Oh

transformHp : (Integer -> Integer) -> Hp -> Hp
transformHp f (MkHp ((currentHp**(maxHp**prf)),baseHp)) =
 let m = transformBounded f currentHp in
     case (choose (leq m maxHp)) of
          Left proofUpperBound => MkHp ((m ** (maxHp ** proofUpperBound)),baseHp)
          Right _ => MkHp (((extendLowerBound maxHp Oh) ** (maxHp ** (maxHpLEQmaxHp maxHp))),baseHp)


{-

True
                and
leq (extendLowerBound (MkBounded (maxHp ** prfMaxHpBounded)) Oh) (MkBounded (maxHp ** prfMaxHpBounded))




-}



{-

transformHp f (MkHp ((currentHp**(maxHp**prf)),baseHp)) =
  let m = transformBounded f currentHp in
      case (choose (leq m maxHp)) of
           Left proofUpperBound => MkHp ((m ** (maxHp ** proofUpperBound)),baseHp)
           Right _ => let c = extendLowerBound maxHp Oh in
                          MkHp ((c ** (maxHp ** (leq_hp_maxhp c maxHp (   ?hole   )))),baseHp)

-}



transformMaxHp : (Integer -> Integer) -> Hp -> Hp
transformMaxHp f (MkHp ((currentHp**(maxHp**prf)),baseHp)) =
  let m = transformBounded f maxHp in
      case (choose (leq currentHp m)) of
           Left proofUpperBound => MkHp ((currentHp ** (m ** proofUpperBound)),baseHp)
           Right _ => let c = extendLowerBound m Oh in
                          MkHp ((c ** (m ** (leq_hp_maxhp c m ( ?hole )))), baseHp)









{-I feel I should be doing something monadic here, but it's perhaps a bit more complicated-}

rest : Hp -> Hp
rest (MkHp((currentHp**(maxHp**prf)),baseHp)) = transformHp (\x => (div4 $ extractBounded maxHp) + x) (MkHp((currentHp**(maxHp**prf)),baseHp))

pierce : Integer {-should this take an integer or a bounded?-} -> Hp -> Hp
pierce val (MkHp((currentHp**(maxHp**prf)),baseHp)) = transformHp (\x => x - val) (MkHp((currentHp**(maxHp**prf)),baseHp))
















