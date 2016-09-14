module Player
import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import objects_basic
import skill_dsl_data
import skill_dsl
import phase
import clientupdates
import card
%access public export
%default total


Spawn : Type
Spawn = Maybe Card
Soul : Type
Soul = Vect 5 (Maybe Monster) {- again more information could go in the type -}

record Player where
 constructor MkPlayer
 {- board : Vect 9 (Maybe Monster)-}
 board : Vect 3 (Vect 3 (Maybe Monster))
 rowTarget : Vect 3 (Fin 3)
 hand : List Card
 graveyard : List Card
 discard : List Card
 spawn : Spawn
 soul : Soul
 thoughts : Bounded 0 Preliminaries.absoluteUpperBound
 knowledge : Vect 6 (Bounded 0 9)
 temporaryId : String

getLiving : Maybe Monster -> Bool
getLiving Nothing = False
getLiving (Just m) with (aliveness (basic m))
 | Alive = True
 | DeadFresh = False
 | DeadStale = False



{-getRowTarget : Player -> Fin 3 -> Maybe Nat {-This takes a player and a row, and returns the index of the next target, or nothing if there is no valid target (living monster)-}-}

{-
getRowTarget : Fin 3 -> Vect 9 (Maybe Monster) -> Fin 9 -> Fin 9 -> Fin 9 -> Maybe (Fin 9)
getRowTarget FZ m a b c with ((getLiving (index a m)),(getLiving (index b m)),(getLiving (index c m)))
 | (True,_,_) = Just a
 | (False,True,_) = Just b
 | (False,False,True) = Just c
 | (False,False,False) = Nothing
getRowTarget (FS FZ) m a b c with ((getLiving (index a m)),(getLiving (index b m)),(getLiving (index c m)))
 | (_,True,_) = Just b
 | (_,False,True) = Just c
 | (True,False,False) = Just a
 | (False,False,False) = Nothing
getRowTarget (FS (FS FZ)) m a b c with ((getLiving (index a m)),(getLiving (index b m)),(getLiving (index c m)))
 | (_,_,True) = Just c
 | (True,_,False) = Just a
 | (False,True,False) = Just b
 | (False,False,False) = Nothing
 -}

incrementRowTarget : Fin 3 -> Fin 3
incrementRowTarget FZ = FS FZ
incrementRowTarget (FS FZ) = FS (FS FZ)
incrementRowTarget (FS (FS FZ)) = FZ
incrementRowTarget (FS (FS (FS FZ))) impossible
incrementRowTarget (FS (FS (FS (FS _)))) impossible


getRowTarget : Fin 3 -> Player -> Maybe (Fin 3)
getRowTarget row player =
  let playerRow = index row (board player) in
      let target = index row (rowTarget player) in
          case index target playerRow of
               Nothing => Nothing
               Just _ => Just row






proveLTE : (n : Nat) -> (i : Fin n) -> LTE (finToNat i) n
proveLTE _ FZ = LTEZero
proveLTE (S k) (FS fk) = LTESucc (proveLTE k fk)


plusOneSucc' : (right : Nat) -> S right = right + 1
plusOneSucc' n = rewrite plusCommutative n 1 in Refl
{-
foobar : (n : Nat) -> Fin n -> Vect n a -> Vect n a
foobar _ FZ x = x
foobar (S k') (FS k) (x::xs) = rewrite plusOneSucc' k' in ((foobar k' k xs) ++ [x])
-}


foobar : Fin n -> Vect n a -> Vect n a
foobar FZ x = x
foobar {n = S k'} (FS k) (x::xs) = foobar (weaken k) (rewrite plusOneSucc' k' in (xs ++ [x]))






{-
barfoo : Fin n -> Vect (n + m) -> Vect n a
barfoo FZ x = x
barfoo {n = S k'} (FS k) (x::xs) = barfoo (weaken k) (rewrite plusOneSucc' k in (xs ++ [x]))
-}


{-edwinb code :D-}

{-Okay, I know this is Edwin code, but if I can't give this a better type I don't think anything else is going to work...-}

front : (i : Fin n) -> Vect n a -> Vect (finToNat i) a
front FZ _ = []
front (FS k) (x :: xs) = x :: (front k xs)


{-ideally I should use (-) here instead of minus, as minus does not require finToNat i to be LTE n-}
back : (i : Fin n) -> Vect n a -> Vect ((-) n (finToNat i) {smaller = proveLTE n i}) a
back FZ xs = xs
back (FS k) (x :: xs) = back k xs

{-
front : (i : Fin (S n)) -> Vect (finToNat i + m) a -> Vect (finToNat i) a
front FZ xs = []
front {n = S p} {m} (FS k) (x :: xs) = x :: front {n = p} {m} k xs

back : (i : Fin (S n)) -> Vect (finToNat i + m) a -> Vect m a
back FZ xs = xs
back {n = S p} {m} (FS k) (x :: xs) = back {n = p} {m} k xs
-}

{-split : (i : Fin (S n)) -> Vect (finToNat i + m) a -> (Vect (finToNat i) a, Vect m a)-}
split : (i : Fin n) -> Vect n a -> (Vect (finToNat i) a, Vect ((-)  n (finToNat i) {smaller = proveLTE n i}) a) {-again, (-) would be better than minus-}
split fin vect = (front fin vect, back fin vect)


findJust : Vect n (Maybe a) -> Maybe (Fin n)
findJust vect = findIndex isJust vect

pullOutSucc : (m : Nat) -> (k : Nat) -> S (plus k m) = plus m (S k)
pullOutSucc m k = ?hole

{-
{- this might have to be Fin (S n), etc... -}
finFun : {n : Nat} -> {m : Nat} -> Fin n -> Fin m -> Fin (n + m)
finFun {n = S k} {m=m} FZ fin = rewrite pullOutSucc m k in (weakenN (S k) fin) {- this is off by 1, since FZ is still the first element -}

I actually do not need this: Either the value is in the left vector (and can be weakened) or it is in the right and can be shifted.

-}







{- more cases .... the above case maybe needs more too? implicits seem suspect -}





{-

myFindJust : Vect n (Maybe a) -> Vect m (Maybe a) -> Maybe (Fin (n + m))
myFindJust vect1 vect2 = case findIndex isJust vect2 of
                              Just f -> 


-}

jj : (n : Nat) -> (k : Nat) -> plus n (S k) = plus (S n) k
jj n k = ?hole

shiftFin : Fin n -> (m : Nat) -> Fin (n + m)
shiftFin {n=n} fin Z = rewrite plusZeroRightNeutral n in fin
shiftFin {n=n} fin (S k) = rewrite jj n k in (shiftFin (FS fin) k)



myFindJust : Vect n (Maybe a) -> Vect m (Maybe a) -> Maybe (Fin (n + m))
myFindJust {n=n} {m=m} vect1 vect2 = case findIndex isJust vect2 of
                              Just i => rewrite plusCommutative n m in (Just (shiftFin i n))
                              Nothing => case findIndex isJust vect1 of
                                              Just i => Just (weakenN m i)
                                              Nothing => Nothing



myFindJust2 : (Vect n (Maybe a), Vect m (Maybe a)) -> Maybe (Fin (n + m))
myFindJust2 (v1,v2) = myFindJust v1 v2


cancelMinus : (n : Nat) -> (i : Nat) -> {auto smaller : LTE i n} -> i + (n - i) = n
cancelMinus n i = ?hole

realCancelMinus : (fin : Fin n) -> (n : Nat) -> {auto smaller : LTE (finToNat fin) n} -> n = (finToNat fin) + (n - (finToNat fin))
realCancelMinus fin n = ?hole


myFindJust1 : Fin n -> Vect n (Maybe a) -> Maybe (Fin n)
myFindJust1 {n=n} fin vect = rewrite realCancelMinus fin n {smaller = proveLTE n fin} in (myFindJust2 (split fin vect))


{-
myFindJust1 {n = n} fin vect = {-rewrite cancelMinus n (finToNat fin) {smaller = proveLTE n fin} in-}  (let (v1,v2) = split fin vect in ({-rewrite cancelMinus n (finToNat fin) {smaller = proveLTE n fin} in-} (myFindJust v1 v2)))
-}

{- still working here!! -}



{-


 Type mismatch between
                 plus (finToNat fin) (n - finToNat fin)
                         and
                                         n



-}





{-
The problem here again seems to be linked to knowing LTE (finToNat fin) n
player.idr:195:38:
When checking right hand side of Player.case block in myFindJust1 at player.idr:195:38 with expected type
        Maybe (Fin (S k))
Type mismatch between
        Maybe (Fin (finToNat fin + minus (S k) (finToNat fin))) (Type of myFindJust v1 v2)
and
        Maybe (Fin (S k)) (Expected type)
Specifically:
        Type mismatch between
                plus (finToNat fin) (minus (S k) (finToNat fin))
        and
                S k

-}


























{-


 (i : Fin (S n)) -> Vect (finToNat i + m) a -> (Vect (finToNat i) a, Vect m a)


 -}









{-
splitVect : (i : Fin (S n)) -> Vect (finToNat i + m) a -> Vect (finToNat i) a
splitVect FZ xs = []
splitVect {n = S n} {m} (FS k) (x :: xs) = x :: splitVect {n} {m} k xs
-}

{-
myprf : plus 0 0 = 0
myprf = Refl

splitVect : {n : Nat} -> Fin (S n) -> Vect (n + m) a -> Vect n a
splitVect {n = Z} {m = Z} FZ [] = the (Vect (n + m) a) []
-}

{-
splitVect : Fin (S n) -> Vect (n + m) a -> (Vect n a, Vect m a)
splitVect {n = Z} FZ v = ([],v)
-}



{-
natMinusFin : (n : Nat) -> Fin n -> Nat
natMinusFin {n = S k'} (S k') FZ = S k'
natMinusFin {n = S k'} (S k') (FS k) = natMinusFin k' k
-}



{-
splitBefore : (k : Fin n) -> Vect n a -> Vect (natMinusFin n k) a
splitBefore FZ x
-}


























{-
par : Fin 3 -> Vect 3 Integer -> (Vect a Integer, Vect b Integer)
par i v = splitAt (finToNat i) v
-}

damageCard : Integer -> Fin 3 -> Fin 3 -> Player -> (List ClientUpdate, Player)

applyAttack : Bounded 0 Preliminaries.absoluteUpperBound -> Fin 3 -> Player -> (List ClientUpdate, Player)
applyAttack atk row defendingPlayer = ?hole



{-
This doesn't quite work. I need to move to the next position AFTER the next card....
-}

{-

goToNextRowTarget : Player -> Fin 3 -> Player
goToNextRowTarget player n = case n of
                              FZ => record {rowTarget = replaceAt FZ (transformRowTarget) (rowTarget player))(rowTarget player)} player
                              FS FZ => record {rowTarget = (rowTarget player)} player
                              FS (FS FZ) => record {rowTarget = (rowTarget player)} player

-}


{-
goToNextRowTarget player n = case n of with (take 3 (drop (3)(board player)))
 | _ = player
-}




syntax "new" "player" [token] = MkPlayer (Vect.replicate 3 (Vect.replicate 3 Nothing)) [FZ,FZ,FZ] [] [] [] Nothing (Vect.replicate 5 Nothing) (>> 0 <<) (Vect.replicate 6 (>> 0 <<)) token

