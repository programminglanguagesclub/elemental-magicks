module Player
import Data.Vect
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import objects_basic
import skill_dsl_data
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
 spawnCard : Spawn
 soulCards : Soul
 thoughtsResource : Bounded 0 Preliminaries.absoluteUpperBound
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

indexLTECardinality : (n : Nat) -> (i : Fin n) -> LTE (finToNat i) n
indexLTECardinality _ FZ = LTEZero
indexLTECardinality (S k) (FS fk) = LTESucc (indexLTECardinality k fk)

{-
plusOneSucc' : (right : Nat) -> S right = right + 1
plusOneSucc' n = rewrite plusCommutative n 1 in Refl

foobar : Fin n -> Vect n a -> Vect n a
foobar FZ x = x
foobar {n = S k'} (FS k) (x::xs) = foobar (weaken k) (rewrite plusOneSucc' k' in (xs ++ [x]))
-}


leftVect : (i : Fin n) -> Vect n a -> Vect (finToNat i) a
leftVect FZ _ = []
leftVect (FS k) (x :: xs) = x :: (leftVect k xs)

rightVect : (i : Fin n) -> Vect n a -> Vect ((-) n (finToNat i) {smaller = indexLTECardinality n i}) a
rightVect FZ xs = xs
rightVect (FS k) (x :: xs) = rightVect k xs

split : (i : Fin n) -> Vect n a -> (Vect (finToNat i) a, Vect ((-)  n (finToNat i) {smaller = indexLTECardinality n i}) a)
split fin vect = (leftVect fin vect, rightVect fin vect)

swapSuccessor : (n : Nat) -> (k : Nat) -> plus n (S k) = plus (S n) k
swapSuccessor Z k = Refl
swapSuccessor (S n') k = rewrite (swapSuccessor n' k) in Refl

shiftFin : Fin n -> (m : Nat) -> Fin (n + m)
shiftFin {n=n} fin Z = rewrite plusZeroRightNeutral n in fin
shiftFin {n=n} fin (S k) = rewrite swapSuccessor n k in (shiftFin (FS fin) k)

findIndexFromRightThenLeft : (a -> Bool) -> Vect n a -> Vect m a -> Maybe (Fin (n + m))
findIndexFromRightThenLeft {n=n} {m=m} f leftVect rightVect = case findIndex f rightVect of
                              Just i => rewrite plusCommutative n m in (Just (shiftFin i n))
                              Nothing => case findIndex f leftVect of
                                              Just i => Just (weakenN m i)
                                              Nothing => Nothing

myFindJust2 : (Vect n (Maybe a), Vect m (Maybe a)) -> Maybe (Fin (n + m))
myFindJust2 (v1,v2) = findIndexFromRightThenLeft isJust v1 v2

cancelMinus : (n : Nat) -> (i : Nat) -> {auto smaller : LTE i n} -> i + (n - i) = n
cancelMinus n i = ?hole

realCancelMinus : (fin : Fin n) -> (n : Nat) -> {auto smaller : LTE (finToNat fin) n} -> n = (finToNat fin) + (n - (finToNat fin))
realCancelMinus fin n = ?hole

myFindJust1 : Fin n -> Vect n (Maybe a) -> Maybe (Fin n)
myFindJust1 {n=n} fin vect = rewrite realCancelMinus fin n {smaller = indexLTECardinality n fin} in (myFindJust2 (split fin vect))


{- written by the mighty Melvar -}
findIndexFrom : (a -> Bool) -> Fin n -> Vect n a -> Maybe (Fin n)
findIndexFrom p FZ xs = findIndex p xs
findIndexFrom p (FS k) (x :: xs) = FS <$> findIndexFrom p k xs

{- written by the mighty Melvar -}
findIndexPreferentiallyFrom : (a -> Bool) -> Fin n -> Vect n a -> Maybe (Fin n)
findIndexPreferentiallyFrom p FZ xs =  findIndex p xs
findIndexPreferentiallyFrom p (FS k) (x :: xs) = if p x then FS <$> findIndexFrom p k xs <|> Just FZ else FS <$> findIndexPreferentiallyFrom p k xs


actualAlive : Maybe Monster -> Bool
actualAlive Nothing = False
actualAlive (Just monster) with (aliveness $ basic monster)
  | DeadFresh = False
  | DeadStale = False
  | Alive = True

findNextLivingMonster : Fin n -> Vect n (Maybe Monster) -> Maybe (Fin n)
findNextLivingMonster fin vect = findIndexPreferentiallyFrom actualAlive fin vect

































{-
findIndexFrom : Fin n -> (a -> Bool) -> Vect n a -> Maybe (Fin n)

findIndexFromWrap : Fin n -> (a -> Bool) -> Vect n a -> Maybe (Fin n)
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

