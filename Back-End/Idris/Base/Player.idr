module Base.Player
import Data.Vect
import Base.BoundedList
import Base.Bounded
import Base.Preliminaries
import Base.Objects_basic
import Base.Clientupdates
import Base.Card
%access public export
%default total


{-
record Player where
 constructor MkPlayer
 {- board : Vect 9 (Maybe Monster)-}
 board : Vect 3 (Vect 3 (Maybe Monster))
 rowTarget : Vect 3 (Fin 3)
 {-hand : BoundedList 25 Card-}
 hand : List Card
 graveyard : List Card
 discard : List Card
 spawnCard : Maybe Card
 soulCards : Vect 5 (Maybe Monster)
 thoughtsResource : Bounded 0 Preliminaries.absoluteUpperBound
 knowledge : Vect 6 (Bounded 0 9)
 temporaryId : String
 -}

record NormalPlayer where
 constructor MkNormalPlayer
 board : Vect 3 (Vect 3 (Maybe Monster))
 rowTarget : Vect 3 (Fin 3)
 hand : List Card
 graveyard : List Card
 discard : List Card
 spawnCard : Maybe Card
 soulCards : Vect 5 Monster
 thoughtsResource : Bounded 0 Preliminaries.absoluteUpperBound
 knowledge : Vect 6 (Bounded 0 9)
 temporaryId : String

record DrawPlayer where
 constructor MkDrawPlayer
 hand : BoundedList 25 Card
 soulCards : Vect 5 (Maybe Monster)
 temporaryId : String



{-
vectToBoundedList : Vect n a -> BoundedList n a
vectToBoundedList [] = Nil
vectToBoundedList (x::xs) = x::(vectToBoundedList xs)
-}


weakenS : BoundedList n a -> BoundedList (S n) a
weakenS [] = []
weakenS (x::xs) = x::(weakenS xs)
{-
weakenM : BoundedList n a -> BoundedList (m + n)
-}

filterVectToBoundedList : Vect n a -> (a -> Bool) -> BoundedList n a
filterVectToBoundedList [] _ = Nil
filterVectToBoundedList (x::xs) f =
 if f x
  then x::(filterVectToBoundedList xs f)
  else weakenS $ filterVectToBoundedList xs f

filterVectMaybes : Vect n (Maybe a) -> BoundedList n a
filterVectMaybes [] = Nil
filterVectMaybes (Nothing::xs) = weakenS $ filterVectMaybes xs
filterVectMaybes ((Just x)::xs) = x::(filterVectMaybes xs)


vectCount : Vect n a -> (a -> Bool) -> Fin (S n)
vectCount [] _ = FZ
vectCount (x::xs) f = if f x then FS (vectCount xs f) else weaken $ vectCount xs f

{-
boundedListToVect : BoundedList (n + m) a -> Vect m a
boundedListToVect Nil = []
-}


blarg : (m : Nat ) -> (n : Nat) -> plus m (S n) = S (m + n)
blarg m n = rewrite plusAssociative m 1 n in (rewrite plusCommutative m 1 in Refl)


concatBoundedList : {n : Nat} -> {m : Nat} -> BoundedList n a -> BoundedList m a -> BoundedList (m + n) a
concatBoundedList [] l2 = weaken l2
concatBoundedList {n=S n1} {m=m} (x::xs) l2 = rewrite blarg m n1 in x::(concatBoundedList xs l2)


getAll : Vect n1 (Maybe a) -> Vect n2 (Maybe a) -> BoundedList n3 b -> BoundedList n4 b -> (a->b) ->  BoundedList ((n1+n2)+(n3+n4)) b
getAll v1 v2 l1 l2 f =
 let x1 = filterVectMaybes v1 in
 let x2 = filterVectMaybes v2 in
 let x3 = map f (concatBoundedList x2 x1) in
 let x4 = concatBoundedList l2 l1 in
 concatBoundedList x4 x3        

doIt : Vect 5 (Maybe Monster) -> Vect 5 (Maybe Monster) -> BoundedList 25 Card -> BoundedList 25 Card -> BoundedList 60 Card
doIt v1 v2 l1 l2 = getAll v1 v2 l1 l2 (\x => MonsterCard x)


filterVectMaybeToList : Vect n (Maybe Monster) -> List Monster
filterVectMaybeToList [] = []
filterVectMaybeToList (Nothing::xs) = filterVectMaybeToList xs
filterVectMaybeToList ((Just x)::xs) = x :: (filterVectMaybeToList xs)

getAllList : Vect n1 (Maybe Monster) -> Vect n2 (Maybe Monster) -> List Card -> List Card -> (Monster -> Card) -> List Card

getAllList v1 v2 l1 l2 f =
 let x1 = filterVectMaybeToList v1 in
 let x2 = filterVectMaybeToList v2 in
 let x3 = map f (x2 ++ x1) in
 let x4 = l2 ++ l1 in
 x4 ++ x3

getAllCardsDrawn : Vect 5 (Maybe Monster) -> Vect 5 (Maybe Monster) -> List Card -> List Card -> List Card
getAllCardsDrawn soulA soulB handA handB = getAllList soulA soulB handA handB (\x => MonsterCard x)


{-concatBoundedList {n=S n1} {m=m} (x::xs) l2 = rewrite blarg m (S n) in x::(concatBoundedList xs l2)-}





























finSum : Fin n -> Fin m -> Fin (n + m)
finSum FZ _ = FZ
finSum (FS k) x = FS (finSum k x)
{-
finSum3 : Fin n1 -> Fin n2 -> Fin n3 -> Fin (n1 + (n2 + n3))
finSum3 FZ x y = finSum x y
finSum3 (FS k) x y = FS (finSum3 k x y)

finSum4 : Fin n1 -> Fin n2 -> Fin n3 -> Fin n4 -> Fin (n1 + (n2 + (n3 + n4)))
-}


{- might want to integrate this -}
record DrawPhasePlayer where
 constructor MkDrawPhasePlayer
 hand : BoundedList 25 Card
 soulCards : Vect 5 (Maybe Monster)
 temporaryId : String


flattenBoard : Vect 3 (Vect 3 (Maybe Monster)) -> Vect 9 (Maybe Monster)
flattenBoard [row0, row1, row2] = row0 ++ row1 ++ row2

unflattenBoard : Vect 9 (Maybe Monster) -> Vect 3 (Vect 3 (Maybe Monster))
unflattenBoard [p0,p1,p2,p3,p4,p5,p6,p7,p8] = [[p0,p1,p2],[p3,p4,p5],[p6,p7,p8]]

idMatches : Nat -> Maybe Monster -> Bool
idMatches monsterId Nothing = False
idMatches monsterId (Just monster) = (id $ basic monster) == monsterId

findBoardMonsterIndex : Nat -> Vect 3 (Vect 3 (Maybe Monster)) -> Maybe (Fin 9)
findBoardMonsterIndex monsterId board = findIndex (idMatches monsterId) (flattenBoard board)

findBoardMonster : Nat -> Vect 3 (Vect 3 (Maybe Monster)) -> Maybe Monster
findBoardMonster monsterId board = case findBoardMonsterIndex monsterId board of
                                        Nothing => Nothing
                                        Just monsterIndex => Vect.index monsterIndex (flattenBoard board)

getLiving : Maybe Monster -> Bool
getLiving Nothing = False
getLiving (Just m) with (aliveness (basic m))
 | Alive = True
 | DeadFresh = False
 | DeadStale = False







{-getRowTarget : Player -> Fin 3 -> Maybe Nat {-This takes a player and a row, and returns the index of the next target, or nothing if there is no valid target (living monster)-}-}

{-
getRowTarget : Fin 3 -> Vect 9 (Maybe Monster) -> Fin 9 -> Fin 9 -> Fin 9 -> Maybe (Fin 9)
getRowTarget FZ m a b c with ((getLiving (Vect.index a m)),(getLiving (Vect.index b m)),(getLiving (Vect.index c m)))
 | (True,_,_) = Just a
 | (False,True,_) = Just b
 | (False,False,True) = Just c
 | (False,False,False) = Nothing
getRowTarget (FS FZ) m a b c with ((getLiving (Vect.index a m)),(getLiving (Vect.index b m)),(getLiving (Vect.index c m)))
 | (_,True,_) = Just b
 | (_,False,True) = Just c
 | (True,False,False) = Just a
 | (False,False,False) = Nothing
getRowTarget (FS (FS FZ)) m a b c with ((getLiving (Vect.index a m)),(getLiving (Vect.index b m)),(getLiving (Vect.index c m)))
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
  let playerRow = Vect.index row (board player) in
      let target = Vect.index row (rowTarget player) in
          case Vect.index target playerRow of
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







usedDeathSkill : Monster -> Bool
usedCounterSkill : Monster -> Bool

damageCard' : Integer -> Monster -> Monster

indexMonster : Fin 3 -> Fin 3 -> Player -> Maybe Monster
damageCard : Integer -> Fin 3 -> Fin 3 -> Player -> (List ClientUpdate, Player)
damageCard val row column player with (indexMonster row column player)
  | Nothing = ([], player)
  | Just monster = let damagedMonster = damageCard' val monster in ?hole {- if hp > 0, and has counter skill, then see if counter skill has been used. otherwise same with death skill. -}

{-This requires loading skills onto the skillQueue, so it really needs more than just a player as input...
 This should be changed to have Game and WhichPlayer as input.
-}




applyAttack : Bounded 0 Preliminaries.absoluteUpperBound -> Fin 3 -> Player -> (List ClientUpdate, Player)
applyAttack atk row defendingPlayer = ?hole



{-This doesn't quite work. I need to move to the next position AFTER the next card....-}

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


syntax "new" "player" [playerId] = MkPlayer (Vect.replicate 3 (Vect.replicate 3 Nothing)) [FZ,FZ,FZ] [] [] [] Nothing (Vect.replicate 5 Nothing) (>> 0 <<) (Vect.replicate 6 (>> 0 <<)) playerId

