module Base.Hp
import Data.So
import Base.Utility
import Base.Bounded
import Base.Preliminaries
%access public export
%default total
{-if I made this a GADT with a single constructor, then I can't pattern match on it apparently. This seems like a bug, although it doesn't matter-}
data Hp = MkHp ((currentHp:Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound**(maxHp:Bounded 0 Preliminaries.absoluteUpperBound**So(leq currentHp maxHp))),{-baseHp:-}Bounded 1 Preliminaries.absoluteUpperBound)


generateHp : Bounded 1 Preliminaries.absoluteUpperBound -> Hp

syntax mkHp [hp] = MkHp (( >> hp << **( >> hp << ** Oh)), >> hp << )

testHp : Hp
testHp = mkHp 20

leq_hp_maxhp : (currentHp : Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound) -> (maxHp : Bounded 0 Preliminaries.absoluteUpperBound) -> So((extractBounded currentHp) <= (extractBounded maxHp)) -> So(leq currentHp maxHp)
leq_hp_maxhp currentHp maxHp _ = believe_me Oh


{-the above is very hard to use...-}

getCurrentHp : Hp -> Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound
getCurrentHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = currentHp
getMaxHp : Hp -> Bounded 0 Preliminaries.absoluteUpperBound
getMaxHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = maxHp
getBaseHp : Hp -> Bounded 1 Preliminaries.absoluteUpperBound
getBaseHp (MkHp((currentHp**(maxHp**prf)),baseHp)) = baseHp
maxHpLEQmaxHp : (maxHp : Bounded 0 Preliminaries.absoluteUpperBound) -> So(leq (the (Bounded Preliminaries.absoluteLowerBound Preliminaries.absoluteUpperBound) (extendLowerBound maxHp Oh)) maxhp)
maxHpLEQmaxHp maxHp = believe_me Oh {-assumes that Preliminaries.absoluteLowerBound will never be strictly positive.-}
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

pierce : Integer {-should this take an integer or a bounded?-} -> Hp -> Hp
pierce val (MkHp((currentHp**(maxHp**prf)),baseHp)) = transformHp (\x => x - val) (MkHp((currentHp**(maxHp**prf)),baseHp))


