module Base.Player

import Data.So
import Data.Vect
import Base.BoundedList
import Base.Bounded
import Base.Preliminaries
import Base.Objects_basic
import Base.Clientupdates
import Base.Card
import Base.Skill_dsl_data

import Pruviloj.Derive.DecEq

%access public export
%default total


%language ElabReflection

orderingDecEq : (x , y : Ordering) -> Dec (x = y)
%runElab deriveDecEq `{orderingDecEq}

DecEq Ordering where
   decEq x y = orderingDecEq x y


{-
data UniqueVect : Vect n (Fin 25) -> Type where
  UniqueEmpty : UniqueVect []
  UniqueConcat : UniqueVect xs -> find (==x) xs = Nothing -> UniqueVect (x::xs)
  -}
dog : a = b -> b = a

data UniqueVect : Vect n (Fin 25) -> Type where
  UniqueEmpty : UniqueVect []
  UniqueConcat : Not (Elem x xs) -> UniqueVect xs -> UniqueVect (x :: xs)


----isElem : DecEq a => (x : a) -> (xs : Vect n a) -> Dec (Elem x xs)

isUniqueVect : (v : Vect n (Fin 25)) -> Dec (UniqueVect v)
isUniqueVect {n=Z} [] = Yes UniqueEmpty
isUniqueVect {n = S k} (x::xs) with (isElem x xs)
  | Yes prfy = ?hole
  | No prfn = ?hole --with (UniqueVect xs)
   -- | Yes prf'y = ?hole
   -- | No prf'n = ?hole




findWhere : DecEq a => (x : a) -> (xs : Vect n a) -> (Elem x xs) -> (i ** index i xs = x)
findWhere x (x::xs) Here = (FZ ** Refl)
findWhere x [] _ impossible
findWhere x (y::xs) (There somewhere) = let foo = findWhere x xs somewhere in ?hole -- WHATEVER!!

-- I want a notion of what it means to not be unique.
-- If you are not unique, there should be two different indices with the same element!
-- I assume for this contradiction only that the 

aaa : (FZ = FS i) -> Void

--aba : (\i1 => Data.Vect.index i1 (y :: z) = x) i -> x = Data.Vect.index i (y :: z)

agas : (UniqueVect (x::xs) -> Void) -> (Elem x xs -> Void) -> UniqueVect xs -> Void

agh : (i = j -> Void) -> (FS i = FS j -> Void)

yoo : (x : a) -> (xs : Vect (S n) a) -> (i : Fin (S n)) -> (j : Fin (S n)) -> Vect.index i xs = Vect.index j xs -> Vect.index (FS i) (x::xs) = Vect.index (FS j) (x::xs)


aff : (v : Vect (S n) (Fin 25)) -> (UniqueVect v -> Void) -> DPair (Fin (S n), Fin (S n)) (\i => (fst i = snd i -> Void, index (fst i) v = index (snd i) v))

aff [x] notUniqueV with (isElem x [])
  | Yes prf impossible
  | No prf = void $ notUniqueV $ UniqueConcat prf UniqueEmpty

aff (x::y::z) notUniqueV with (isElem x (y::z))
  | Yes prf = let (i** p) = findWhere x (y::z) prf in ((FZ, FS i) ** (aaa, dog p))
  | No prf = let ((i1,i2) ** (littleAffProof1, littleAffProof2)) = aff (y::z) (agas notUniqueV prf) in ((FS i1,FS i2) ** (agh littleAffProof1, yoo x (y::z) i1 i2 littleAffProof2))




uniqueRemoveHead : (l : Vect (S n) (Fin 25)) -> UniqueVect l -> UniqueVect (tail l)
uniqueRemoveHead (x::xs) (UniqueConcat uniqueH uniqueT) = uniqueT

deleteAtHeadRemovesHead : (l : Vect (S n) (Fin 25)) -> deleteAt FZ l = tail l

onlyOneEmpty : (v : Vect 0 (Fin 25)) -> v = []
onlyOneEmpty [] = Refl
onlyOneEmpty (x::xs) impossible

{-
deleteInward : (v : Vect (S (S n)) (Fin 25)) -> (fk : Fin (S n)) -> deleteAt (FS fk) v = (head v) :: (deleteAt fk (tail v))
-}

--undoDelete 

deleteAtHead : (v : Vect (S n) (Fin 25)) -> deleteAt FZ v = tail v
deleteAtHead [] impossible
deleteAtHead (x::xs) = Refl


deleteInTail : (v : Vect (S (S n)) (Fin 25)) -> (fk : Fin (S n)) -> head (deleteAt (FS fk) v) = head v
deleteInTail [] i impossible
deleteInTail (x::xs) i = Refl


deleteLemma : (v : Vect (S (S n)) (Fin 25)) -> (fk : Fin (S n)) -> deleteAt (FS fk) v = head v :: (deleteAt fk (tail v))

uniqueRemove : (l : Vect (S n) (Fin 25)) -> (i : Fin (S n)) -> UniqueVect l -> UniqueVect (deleteAt i l)
uniqueRemove l FZ uniqueL = rewrite deleteAtHeadRemovesHead l in uniqueRemoveHead l uniqueL
--uniqueRemove {n = S k} (x::xs) (FS fk) (UniqueConcat uniqueHead uniqueTail) =
uniqueRemove {n = S Z} (x::xs) (FS fk) (UniqueConcat uniqueHead uniqueTail) =
  let uniqueM = uniqueRemove xs fk uniqueTail in
    case deleteAt fk xs of
     [] => UniqueConcat (rewrite onlyOneEmpty (deleteAt fk xs) in noEmptyElem) uniqueM
     (y::ys) impossible

----uniqueRemove {n = S (S k)} (x::xs) (FS fk) (UniqueConcat uniqueHead uniqueTail) with (decEq



-----with (deleteAt fk xs) proof blah


{-
--- goal type : UniqueVect (deleteAt (FS fk) (x::xs))
-- need Not (elem )



| [] impossible
 | (y::ys) =
    let uniqueM = uniqueRemove xs fk uniqueTail in
    case isElem x (y::ys) of
     No prf => UniqueConcat prf (rewrite blah in uniqueM)
     Yes Here =>
      let q = deleteLemma v fk in ?hole
     Yes There => ?hole
     -}




  --case (decEq (deleteAt fk xs) []) of
    --Yes prf => ?hole -- UniqueConcat noEmptyElem uniqueM
   -- No prf' => ?hole
  --case (decEq (deleteAt fk xs) []) of
    --Yes prf => ?hole -- UniqueConcat ?noEmptyElem uniqueM 
    --No prf' {-(y::ys)-} => ?hole
    {-
  case isElem x (deleteAt fk xs) of
    No prf => UniqueConcat prf uniqueM
    Yes Here => ?hole
    Yes There => ?hole
    -}

               {-
     case uniqueM of
      UniqueEmpty => ?hole -- possibility trap
      UniqueConcat uniqueH uniqueT => ?hole
      -}
               
               
               ---?hole -- when we searched the deleted tail we don't get nothing. Show that if the element was not deleted we would also find something
                                                                                                                               
  
  -- prf' : (find (==x) (deleteAt fk xs) = Nothing) -> Void


{-

gh : (l : Vect n (Fin 25)) -> UniqueVect l = UniqueVect (l ++ []) 
hg : (l : Vect n (Fin 25)) -> UniqueVect l = UniqueVect ([] ++ l)
hg l = Refl

--gah : UniqueVect l = UniqueVect (l ++ [])
--gah = philipCast f ?hole ?hole
{-
ges : (n : Nat) -> Type
ges n = Vect n (Fin 25)


blara : Vect n (Fin 25) = Vect (plus n 0) (Fin 25)
blara = philipCast q ges Refl

-}


-- this worked... then just suddenly stopped working...!!
{-
uniqueConcat2 : (l : Vect n (Fin 25)) -> (k : Vect m (Fin 25)) -> UniqueVect (l ++ k) -> UniqueVect l
uniqueConcat2 l [] lkUnique = rewrite gh l in lkUnique -----rewrite (g $ appendNilRightNeutral l) in (rewrite (gh l) in lkUnique) -- put {n=n}{m=m} into scope?
                              -}



--uniqueConcat l (kh::kt) lkUnique with (lkUnique)
--  uniqueConcat {n=n} {m=Z} l (kh::kt)   | UniqueEmpty = ?hole --impossible
-- uniqueConcat {n=n} {m=S m'} l (kh::kt) | UniqueConcat lktUnique lkhUnique = ?hole --uniqueConcat l kh lktUnique


gkj : (x : Fin 25) -> (v : Vect n (Fin 25)) -> (w : Vect m (Fin 25)) -> find (==x) (v++w) = Nothing -> find (==x) v = Nothing

uniqueConcat : (l : Vect n (Fin 25)) -> (k : Vect m (Fin 25)) -> UniqueVect (k ++ l) -> UniqueVect l
uniqueConcat l [] klUnique = rewrite hg l in klUnique
uniqueConcat l (kh::kt) klUnique with (klUnique)
  | UniqueEmpty impossible
  | UniqueConcat uniqueListT uniqueH = uniqueConcat l kt uniqueListT


notUniqueConcat : (l : Vect n (Fin 25)) -> (k : Vect m (Fin 25)) -> (UniqueVect l -> Void) -> UniqueVect (k++l) -> Void
notUniqueConcat l k notUniqueL uniqueKL = notUniqueL $ uniqueConcat l k uniqueKL


uniqueConcat2 : (l : Vect n (Fin 25)) -> (k : Vect m (Fin 25)) -> UniqueVect (l ++ k) -> UniqueVect l
uniqueConcat2 [] k _ = UniqueEmpty
uniqueConcat2 {n=S n'} {m=m} (lh::lt) k lkUnique with (lkUnique)
  | UniqueEmpty impossible
  | UniqueConcat tailUnique headUnique =
   let uniqueLTail = uniqueConcat2 lt k tailUnique in
   UniqueConcat uniqueLTail (gkj lh lt k headUnique)


hak : (q : Vect a (Fin 25)) -> (l : Vect b (Fin 25)) -> (k : Vect c (Fin 25)) -> (q++l)++k = q++l++k

uniqueConcat3 : (l : Vect n (Fin 25)) -> (k : Vect m (Fin 25)) -> (q : Vect o (Fin 25)) -> UniqueVect (q ++ l ++ k) -> UniqueVect l
uniqueConcat3 l k q prf = let qlUnique = uniqueConcat2 (q++l) k (rewrite hak q l k in prf) in uniqueConcat l q qlUnique

uniqueConcat4 : (l : Vect n (Fin 25)) -> (k : Vect m (Fin 25)) -> (q : Vect o (Fin 25)) -> UniqueVect ((q ++ l) ++ k) -> UniqueVect l
uniqueConcat4 l k q prf = let qlUnique = uniqueConcat2 (q++l) k prf in uniqueConcat l q qlUnique



uniqueRemoveHead : (l : Vect (S n) (Fin 25)) -> UniqueVect l -> UniqueVect (tail l)
uniqueRemoveHead (x::xs) (UniqueConcat uniqueListT uniqueH) = uniqueListT


{-
      Type mismatch between
                      tail l
                              and
                                              deleteAt FZ l
                                              -}


deleteAtHeadRemovesHead : (l : Vect (S n) (Fin 25)) -> deleteAt FZ l = tail l
{-
deleteTailEquality : (x : Fin 25) -> (xs : Vect (S n) (Fin 25)) -> (fk : Fin (S n)) -> x :: (deleteAt fk xs) = deleteAt (FS fk) (x :: xs)
-}

uniqueRemove : (l : Vect (S n) (Fin 25)) -> (i : Fin (S n)) -> UniqueVect l -> UniqueVect (deleteAt i l)
uniqueRemove l FZ uniqueL = rewrite deleteAtHeadRemovesHead l in uniqueRemoveHead l uniqueL
uniqueRemove {n = S k} (x::xs) (FS fk) (UniqueConcat uniqueTail uniqueHead) =
  --let xs' = deleteAt fk xs in
  let uniqueM = uniqueRemove xs fk uniqueTail in
  case (decEq (find (==x) (deleteAt fk xs)) Nothing) of
   Yes prf => UniqueConcat uniqueM prf
   No prf' => ?hole -- when we searched the deleted tail we don't get nothing. Show that if the element was not deleted we would also find something

-- prf' : (find (==x) (deleteAt fk xs) = Nothing) -> Void
-}
-------------------------------------------------------------------------------
record Player where
 constructor MkPlayer
 --board : Vect 3 (Vect 3 (Maybe FieldedMonster)) -- should wrap this in a datatype, and/or use matrix..
 board : Vect 9 (Maybe FieldedMonster)
 rowTarget : Vect 3 (Fin 3)
 hand : (m ** Vect m Card)
 graveyard : (n ** Vect n Card)
 banished : (o ** Vect o Card)
 spawnCard : Maybe Card
 soulCards : BoundedList 5 SoulCard
 thoughtsResource : Bounded 0 Preliminaries.absoluteUpperBound
 knowledge : Vect 6 (Bounded 0 9)
 temporaryId : String
-- should add proof that the total number of cards controlled is 30.
 timeRemainingMilliseconds : Nat



--getBoardIds : Vect q (Maybe FieldedMonster) -> (n ** Vect n (Fin 25))
--getBoardIds [] = (0 ** [])
--getBoardIds (Nothing::xs) = getBoardIds xs
--getBoardIds ((Just fm) ::xs) = let (n ** v) = getBoardIds xs in (1+n ** (id $ basic fm) :: v)


--combineVectors : (n ** Vect n (Fin 25)) -> (m ** Vect m (Fin 25)) -> (n+m ** Vect (n+m) (Fin 25))
--combineVectors : DPair Nat (\n => Vect n (Fin 25)) -> DPair Nat (\m => Vect m (Fin 25)) -> DPair Nat (\_

----concatMaybe : 
{-
data BoardIds : (v : Vect n (Fin 25)) -> Type where
  EmptyBoard : BoardIds []
  ConcatBoardId : (raw : Vect m (Fin 25)) ->
   (w : Vect n (Fin 25)) ->
   case raw of
    [] => BoardIds w
    --_ => BoardIds w

-}
--catMaybes : Vect n (Maybe elem) -> (p ** Vect p elem)












{-
combineVectors :  Vect n (Fin 25) -> Vect m (Fin 25) -> Vect (n+m) (Fin 25)

data HasBoardIds : Vect q (Maybe (Fin 25)) -> (v : Vect n (Fin 25) ** UniqueVect v) -> Type where
  UniqueEmptyBoardIds : HasBoardIds [] ([] ** UniqueEmpty)
  UniqueNothingBoardIds : HasBoardIds xs v -> HasBoardIds (Nothing::xs) v
  UniqueConcatBoardIds : HasBoardIds xs (v**prf) -> (prf2 : find (==x) v = Nothing) -> HasBoardIds ((Just x)::xs) (x::v ** UniqueConcat prf prf2)

data HasSpawnId : Maybe (Fin 25) -> (v : Vect n (Fin 25) ** UniqueVect v) -> Type where
  UniqueEmptySpawnId : HasSpawnId Nothing ([] ** UniqueEmpty)
  UniqueOccupiedSpawnId : HasSpawnId (Just x) ([x] ** UniqueConcat UniqueEmpty Refl)

-}

{-
data HasContainerId : Vect n (Fin 25) -> (v : Vect n (Fin 25) ** UniqueVect v) -> Type where
  UniqueEmptyContainer : HasContainerId [] ([] ** UniqueEmpty)
  UniqueConcatContainer : HasContainerId xs (v**prf) -> (prf2 : find (==x) v = Nothing) -> HasContainerId (x::xs) (x::v ** UniqueConcat prf prf2)
  -}
{-
data HasIds :
 Vect  ->
 HasSpawnId spawn spawnId ->
  hand ->
 UniqueVect graveyard ->
 UniqueVect banished ->
 Type
 where
  
  MkHasIds :
   (hasBoardIds : HasBoardIds board boardIds) ->
   (hasSpawnId : HasSpawnId spawn spawnId) ->
   (hasHandIds : UniqueVect hand) ->
   (hasGraveyardIds : UniqueVect graveyard) ->
   (hasBanishedIds : UniqueVect banished) ->
   UniqueVect ((fst boardIds) ++ (fst spawnId) ++ hand ++ graveyard ++ banished) ->
   HasIds hasBoardIds hasSpawnId hasHandIds hasGraveyardIds hasBanishedIds
          



-}
foodth : (n : Nat) -> S n = plus n 1
{-


moveFromHandToGraveyard' :
 {n : Nat} ->
 (index : Fin (S n)) ->
 (hand : Vect (S n) (Fin 25)) ->
 (graveyard : Vect m (Fin 25)) ->
 (Vect n (Fin 25), Vect (S m) (Fin 25))

moveFromHandToGraveyard' {m=m} FZ (h::hs) graveyard = (hs, rewrite foodth m in graveyard ++ [h])




moveFromHandToGraveyard :
-- {board : HasBoardIds rawBoard boardIds} ->
-- {spawn : HasSpawnId rawSpawn spawnId} ->
-- {n : Nat} ->
-- {v : Vect n (Fin 25)} ->
-- {hand : UniqueVect v} ->
-- {w : Vect m (Fin 25)} ->
-- {graveyard : UniqueVect w} ->
-- {x : Vect l (Fin 25)} ->
-- {banished : UniqueVect x} ->
 (index : Fin n) ->
 HasIds board spawn hand graveyard banished ->
 HasIds board spawn (UniqueVect $ fst $ moveFromHandToGraveyard' index ?hand ?graveyard) (UniqueVect $ snd $ moveFromHandToGraveyard index ?hand ?graveyard) banished

moveFromHandToGraveyard FZ (MkHasIds board spawn hand graveyard banished uniqueVect) = ?hole --(MkHasIds board spawn hand graveyard banished uniqueVect)
moveFromHandToGraveyard (FS fk) (MkHasIds board spawn hand graveyard banished uniqueVect) = ?hole

  -}
 
{-
moveFromHandToGraveyard' :
  {n : Nat} ->
  (index : Fin (S n)) ->
  (hand : Vect (S n) (Fin 25)) ->
  (graveyard : Vect m (Fin 25)) ->
  (Vect n (Fin 25), Vect (S m) (Fin 25))
            
moveFromHandToGraveyard' {m=m} FZ (h::hs) graveyard = (hs, rewrite foodth m in graveyard ++ [h])



-}


  --(MkHasIds board spawn hand graveyard banished uniqueVect) =
  -- ?hole
              
             -- HasSpawnId Nothing ([] ** UniqueEmpty) -> HasIds board boardIds



{-
getBoardIds : Vect q (Maybe FieldedMonster) -> DPair Nat (\n => DPair (Vect n (Fin 25)) (\v => UniqueVect v))
getBoardIds [] = MkDPair 0 (MkDPair [] UniqueEmpty)
getBoardIds (Nothing::xs) = getBoardIds xs
getBoardIds ((Just fm)::xs) = let MkDPair n (MkDPair l prf) = getBoardIds -- WRONG
-}

              ---UniqueVect (snd art))

{-
getBoardIds [] = ([] ** ?hole )--UniqueEmpty)
getBoardIds (Nothing::xs) = ?hole
getBoardIds ((Just fm)::xs) = ?hole
-}
data CorrectPlayer : Player -> Type where
  MkCorrectPlayer :
   (player : Player) -> 
   ((length (snd $ hand player)) + (length (snd $ graveyard player)) + (length (snd $ banished player)) = 2354) ->
   CorrectPlayer player                                                                                  

{-


data CorrectPlayer : Player -> Type where
   MkCorrectPlayer :
     (player : Player) ->
       (length $ getPlayerIdList player = 25) ->
                                           -- ((Base.Card.getId <$> (index' 0 $ hand player)) = Just FZ) ->
                                                                                                  (UniqueList $ getPlayerIdList player) ->
                                                                                                    ((x : Fin 25) -> find (==x) (getPlayerIdList player) = Nothing -> Void) ->
                                                                                                      CorrectPlayer player

                                                                                                      -}


record DrawPlayer where
 constructor MkDrawPlayer
 hand : BoundedList 25 Card -- might want to make this a normal list.
 soulCards : Vect 5 (Maybe SoulCard)
 temporaryId : String
 timeRemainingMilliseconds : Nat
{-
hand : Player -> List Card
hand player =
  let (_ ** cards) = hand1 player in toList cards
graveyard : Player -> List Card
graveyard player =
  let (_ ** cards) = graveyard1 player in toList cards
discard : Player -> List Card
discard player =
  let (_ ** cards) = discard1 player in toList cards
  -}
-------------------------------------------------------------------------------
initialRowTarget : Vect 3 (Fin 3)
initialRowTarget = replicate 3 FZ
-------------------------------------------------------------------------------
initialKnowledge : Vect 6 (Bounded 0 9)
initialKnowledge = replicate 6 $ bind 0
-------------------------------------------------------------------------------
initialThoughts : Bounded 0 Preliminaries.absoluteUpperBound
initialThoughts = bind 5
-------------------------------------------------------------------------------
emptyDiscard : (n ** Vect n Card)
emptyDiscard = (0 ** [])
-------------------------------------------------------------------------------
emptyGraveyard : (n ** Vect n Card)
emptyGraveyard = (0 ** [])
-------------------------------------------------------------------------------
--emptyBoard : Vect 3 (Vect 3 (Maybe FieldedMonster))
--emptyBoard = replicate 3 $ replicate 3 Nothing
emptyBoard : Vect 9 (Maybe FieldedMonster)
emptyBoard = replicate 9 Nothing
-------------------------------------------------------------------------------
emptySoul : Vect 5 (Maybe SoulCard)
emptySoul = replicate 5 Nothing
-------------------------------------------------------------------------------
emptySpawn : Maybe Card
emptySpawn = Nothing
-------------------------------------------------------------------------------
newPlayer :
 String ->
 Vect 5 SoulCard ->
 Vect 25 Card ->
 Nat ->
 Player

newPlayer playerId soul hand timeRemainingMilliseconds =
 MkPlayer
  emptyBoard
  initialRowTarget
  (25 ** hand)
  emptyGraveyard
  emptyDiscard
  emptySpawn
  (fromVect soul)
  initialThoughts
  initialKnowledge
  playerId
  timeRemainingMilliseconds
-------------------------------------------------------------------------------
newDrawPlayer : String -> DrawPlayer
newDrawPlayer playerId = MkDrawPlayer [] emptySoul playerId initialTimeRemainingMilliseconds
-------------------------------------------------------------------------------
weakenS : BoundedList n a -> BoundedList (S n) a
weakenS [] = []
weakenS (x::xs) = x::(weakenS xs)
-------------------------------------------------------------------------------
filterVectToBoundedList : Vect n a -> (a -> Bool) -> BoundedList n a
filterVectToBoundedList [] _ = Nil
filterVectToBoundedList (x::xs) f =
 if f x
  then x::(filterVectToBoundedList xs f)
  else weakenS $ filterVectToBoundedList xs f
-------------------------------------------------------------------------------
filterVectMaybes : Vect n (Maybe a) -> BoundedList n a
filterVectMaybes [] = Nil
filterVectMaybes (Nothing::xs) = weakenS $ filterVectMaybes xs
filterVectMaybes ((Just x)::xs) = x::(filterVectMaybes xs)
-------------------------------------------------------------------------------
vectCount :
 Vect n a ->
 (a -> Bool) ->
 Fin (S n)

vectCount [] _ = FZ
vectCount (x::xs) f =
 if f x
  then FS (vectCount xs f)
  else weaken $ vectCount xs f
-------------------------------------------------------------------------------
pullOutPlus :
 (m : Nat ) ->
 (n : Nat) ->
 plus m (S n) = S (m + n)

pullOutPlus m n =
 rewrite plusAssociative m 1 n in
 (rewrite plusCommutative m 1 in Refl)
-------------------------------------------------------------------------------
concatBoundedList :
 {n : Nat} ->
 {m : Nat} ->
 BoundedList n a ->
 BoundedList m a ->
 BoundedList (m + n) a

concatBoundedList [] l2 = weaken l2
concatBoundedList {n=S n1} {m=m} (x::xs) l2 =
 rewrite pullOutPlus m n1 in
 x :: (concatBoundedList xs l2)
-------------------------------------------------------------------------------
getAll :
 Vect n1 (Maybe a) ->
 Vect n2 (Maybe a) ->
 BoundedList n3 b ->
 BoundedList n4 b ->
 (a->b) ->
 BoundedList ((n1+n2)+(n3+n4)) b

getAll v1 v2 l1 l2 f =
 let x1 = filterVectMaybes v1 in
 let x2 = filterVectMaybes v2 in
 let x3 = map f (concatBoundedList x2 x1) in
 let x4 = concatBoundedList l2 l1 in
 concatBoundedList x4 x3        
-------------------------------------------------------------------------------
buildDrawnCardsList :
 Vect 5 (Maybe SoulCard) ->
 Vect 5 (Maybe SoulCard) ->
 BoundedList 25 Card ->
 BoundedList 25 Card ->
 BoundedList 60 Card

buildDrawnCardsList v1 v2 l1 l2 = ?hole --getAll v1 v2 l1 l2 (\x => MonsterCard x)
-------------------------------------------------------------------------------
{-
filterVectMaybeToList : Vect n (Maybe Monster) -> List Monster
filterVectMaybeToList [] = []
filterVectMaybeToList (Nothing::xs) = filterVectMaybeToList xs
filterVectMaybeToList ((Just x)::xs) = x :: (filterVectMaybeToList xs)
-------------------------------------------------------------------------------
getAllList :
 Vect n1 (Maybe Monster) ->
 Vect n2 (Maybe Monster) ->
 BoundedList 25 Card ->
 BoundedList 25 Card ->
 (Monster -> Card) ->
 List Card

getAllList v1 v2 l1 l2 f = ?hole

{-
 let x1 = filterVectMaybeToList v1 in
 let x2 = filterVectMaybeToList v2 in
 let x3 = map f (x2 ++ x1) in
 let x4 = l2 ++ l1 in
 x4 ++ x3
-}
-------------------------------------------------------------------------------
getAllCardsDrawn : Vect 5 (Maybe Monster) -> Vect 5 (Maybe Monster) -> BoundedList 25 Card -> BoundedList 25 Card -> List Card
getAllCardsDrawn soulA soulB handA handB = getAllList soulA soulB handA handB (\x => MonsterCard x)
-------------------------------------------------------------------------------

finSum : Fin n -> Fin m -> Fin (n + m)
finSum FZ _ = FZ
finSum (FS k) x = FS (finSum k x)
{-
finSum3 : Fin n1 -> Fin n2 -> Fin n3 -> Fin (n1 + (n2 + n3))
finSum3 FZ x y = finSum x y
finSum3 (FS k) x y = FS (finSum3 k x y)

finSum4 : Fin n1 -> Fin n2 -> Fin n3 -> Fin n4 -> Fin (n1 + (n2 + (n3 + n4)))
-}

-}
-------------------------------------------------------------------------------
flattenBoard : Vect 3 (Vect 3 (Maybe FieldedMonster)) -> Vect 9 (Maybe FieldedMonster)
flattenBoard [row0, row1, row2] = row0 ++ row1 ++ row2
-------------------------------------------------------------------------------
unflattenBoard : Vect 9 (Maybe FieldedMonster) -> Vect 3 (Vect 3 (Maybe FieldedMonster))
unflattenBoard [p0,p1,p2,p3,p4,p5,p6,p7,p8] =
 [[p0,p1,p2],[p3,p4,p5],[p6,p7,p8]]
-------------------------------------------------------------------------------

namespace fielded
 idMatches :
  Fin 25 ->
  Maybe FieldedMonster ->
  Bool

 idMatches monsterId Nothing = False
 idMatches monsterId (Just monster) = (id $ basic monster) == monsterId

namespace unfielded
 idMatches :
  Fin 25 ->
  Maybe UnfieldedMonster ->
  Bool

 idMatches monsterId Nothing = False
 idMatches monsterId (Just monster) = (id $ basic monster) == monsterId


-------------------------------------------------------------------------------
findBoardMonsterIndex :
 Fin 25 ->
 Vect 3 (Vect 3 (Maybe FieldedMonster)) ->
 Maybe (Fin 9)

findBoardMonsterIndex monsterId board =
 ?hole --findIndex (idMatches monsterId) ?hole --(flattenBoard board)
-------------------------------------------------------------------------------
findBoardMonster :
 Fin 25 ->
 Vect 3 (Vect 3 (Maybe FieldedMonster)) ->
 Maybe FieldedMonster

findBoardMonster monsterId board =
 case findBoardMonsterIndex monsterId board of
  Nothing => Nothing
  Just monsterIndex => Vect.index monsterIndex ?hole --(flattenBoard board)
-------------------------------------------------------------------------------
incrementRowTarget : Fin 3 -> Fin 3
incrementRowTarget FZ = FS FZ
incrementRowTarget (FS FZ) = FS (FS FZ)
incrementRowTarget (FS (FS FZ)) = FZ
incrementRowTarget (FS (FS (FS FZ))) impossible
incrementRowTarget (FS (FS (FS (FS _)))) impossible
-------------------------------------------------------------------------------
findIndexFrom : (a -> Bool) -> Fin n -> Vect n a -> Maybe (Fin n)
findIndexFrom p FZ xs = findIndex p xs
findIndexFrom p (FS k) (x :: xs) = FS <$> findIndexFrom p k xs
-------------------------------------------------------------------------------
findIndexPreferentiallyFrom :
 (a -> Bool) ->
 Fin n ->
 Vect n a ->
 Maybe (Fin n)

findIndexPreferentiallyFrom p FZ xs = findIndex p xs
findIndexPreferentiallyFrom p (FS k) (x :: xs) =
 if p x
  then
   FS <$> findIndexFrom p k xs <|> Just FZ
  else
   FS <$> findIndexPreferentiallyFrom p k xs
-------------------------------------------------------------------------------
--findWithIndex : DecEq a => (a -> Bool) -> (v : Vect n a) -> Maybe (DPair (Fin n, a) (\(i,e) => (Vect.index i v = e)))
--findWithIndex p [] = Nothing
--findWithIndex p (x::xs) = if p x then Just ((FZ, x) ** Refl) else map (\((i,e) ** prf) => ((FS i,e) ** prf)) $ findWithIndex p xs
-------------------------------------------------------------------------------
--findWithIndexFrom : DecEq a => (a -> Bool) -> Fin n -> (v1 : Vect n a) -> Maybe (DPair (Fin n, a) (\(i1,e1) => (Vect.index i1 v1 = e1)))
--findWithIndexFrom p FZ [x] = if p x then Just ((FZ, x) ** Refl) else Nothing
--findWithIndexFrom p FZ (x1 :: x2 :: xs) = if p x1 then Just ((FZ, x1) ** Refl) else map (\((i,e) ** prf) => ((FS i,e) ** prf)) $ findWithIndexFrom p FZ (x2 :: xs)
--findWithIndexFrom p (FS k) (x1 :: x2 :: xs) =
-- let output = findWithIndexFrom p k (x2 :: xs) in
--  case output of
--   Nothing => Nothing
--   Just ((i_offset, e) ** prf) => Just ((FS i_offset, e) ** prf)
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- TRUSTED COMPONENTS PART 1/2
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- TRUSTED DEFINITION
-------------------------------------------------------------------------------
-- correct the ordering
computeSearchIndex : (begin : Fin n) -> (i : Fin n) -> Fin n
computeSearchIndex FZ i = i
computeSearchIndex (FS k1) (FS k2) = weaken $ computeSearchIndex k1 k2
computeSearchIndex (FS k) FZ = computeSearchIndex (assert_smaller (FS k) $ weaken k) last
-------------------------------------------------------------------------------
-- TRUSTED DEFINITION
-------------------------------------------------------------------------------
leq2 : Ordering -> Bool
leq2 GT = False
leq2 EQ = True
leq2 LT = True
-------------------------------------------------------------------------------
-- UNTRUSTED COMPONENTS
-------------------------------------------------------------------------------
-- IMPLEMENTATION
-------------------------------------------------------------------------------
ugh : (Fin k, a) -> (Fin (S k), a)
ugh (i,e) = (FS i, e)
-------------------------------------------------------------------------------
findWithIndex : DecEq a => (a -> Bool) -> (v : Vect n a) -> Maybe (Fin n, a)
findWithIndex p [] = Nothing
findWithIndex p (x::xs) = if (p x) then Just (FZ, x) else map ugh $ findWithIndex p xs
-------------------------------------------------------------------------------
findWithIndexFrom : DecEq a => (a -> Bool) -> Fin n -> (v1 : Vect n a) -> Maybe (Fin n, a)
findWithIndexFrom p FZ v = findWithIndex p v
findWithIndexFrom p (FS k) (x :: xs) = (\(i,e) => (FS i, e)) <$> findWithIndexFrom p k xs
findWithIndexFrom _ _ [] impossible
------------------------------------------------------------------------------------------------
findWithIndexPreferentiallyFromSimplyTyped :
 DecEq a =>
 (p : a -> Bool) ->
 (begin : Fin n) ->
 (v : Vect n a) ->
 Maybe (Fin n, a)
                       
findWithIndexPreferentiallyFromSimplyTyped p FZ v = findWithIndex p v
findWithIndexPreferentiallyFromSimplyTyped p (FS k) (x::xs) with (p x)
 | True = (\(i,e) => (FS i, e)) <$> findWithIndexFrom p k xs <|> Just (FZ,x)
 | False = (\(i,e) => (FS i, e)) <$> findWithIndexPreferentiallyFromSimplyTyped p k xs
-------------------------------------------------------------------------------
-- PROOFS
-------------------------------------------------------------------------------
foo :
 DecEq a =>
 (p : a -> Bool) ->
 (begin : Fin n) ->
 (v : Vect n a) ->
 Nothing = findWithIndexPreferentiallyFromSimplyTyped p begin v ->
 find p v = Nothing
-------------------------------------------------------------------------------
terrible : False = True -> a
terrible = ?unproven
-------------------------------------------------------------------------------
helper : (p : a -> Bool) -> (x : a) -> ((p x = True) -> Void) -> ((if p x then Just x else Nothing) = Nothing)
helper = ?hole
-------------------------------------------------------------------------------
findWithIndexPreferentiallyFromProof1 :
 DecEq a =>
 (p : a -> Bool) ->
 (begin : Fin n) ->
 (v : Vect n a) ->
 (i : Fin n) ->
 (e : a) ->
 (Just (i,e) = findWithIndexPreferentiallyFromSimplyTyped p begin v) ->
 Vect.index i v = e

findWithIndexPreferentiallyFromProof1 p FZ v i e prf = ?unproven
findWithIndexPreferentiallyFromProof1 p (FS k) v i e prf = ?unproven
-------------------------------------------------------------------------------
findWithIndexPreferentiallyFromProof2 :
 DecEq a =>
 (p : a -> Bool) ->
 (begin : Fin n) ->
 (v : Vect n a) ->
 (i : Fin n) ->
 (e : a) ->
 (Just (i,e) = findWithIndexPreferentiallyFromSimplyTyped p begin v) ->
 Vect.index i v = e ->
 (i' : Fin n) ->
 p (Vect.index i' v) = True ->
 leq2 $ compare (computeSearchIndex begin i) (computeSearchIndex begin i') = True

findWithIndexPreferentiallyFromProof2 = ?unproven
-------------------------------------------------------------------------------
findWithIndexPreferentiallyFromProof :
 DecEq a =>
 (p : a -> Bool) ->
 (begin : Fin n) ->
 (v : Vect n a) ->
 (i : Fin n) ->
 (e : a) ->
 (Just (i,e) = findWithIndexPreferentiallyFromSimplyTyped p begin v) ->
 (Vect.index i v = e, -- property 1) element at index.
  ((i' : Fin n) -> -- property 2) index is first valid.
  p (Vect.index i' v) = True ->
  leq2 $ compare (computeSearchIndex begin i) (computeSearchIndex begin i') = True))

findWithIndexPreferentiallyFromProof p begin v i e prf =
 let prf1 = findWithIndexPreferentiallyFromProof1 p begin v i e prf in
 let prf2 = (\i' => \prf2' => findWithIndexPreferentiallyFromProof2 p begin v i e prf prf1 i' prf2') in
 (prf1, prf2)
-------------------------------------------------------------------------------
nowTheProof : (i2 : Fin (S k)) -> leq2 $ compare (computeSearchIndex FZ FZ) (computeSearchIndex FZ i2) = True
nowTheProof {k=k} i2 with (computeSearchIndex FZ i2)
 | FZ with (leq2 $ compare (computeSearchIndex (the (Fin (S k)) FZ) (the (Fin (S k)) FZ)) (the (Fin (S k)) FZ))
  | True = Refl
  | False = ?hole
 | FS fk with (leq2 $ compare (computeSearchIndex (the (Fin (S k)) FZ) (the (Fin (S k)) FZ)) (FS fk))
  | True = Refl
  | False = ?hole
-------------------------------------------------------------------------------
proof1 :
 DecEq a =>
 (p : a -> Bool) ->
 (v : Vect (S k) a) ->
 (i : Fin (S k)) ->
 (e : a) ->
 (findWithIndexPreferentiallyFromSimplyTyped p FZ v = findWithIndex p v)

proof1 p v i e = ?unproven
-------------------------------------------------------------------------------
lemma2 :
 (witness : DecEq a) =>
 (p : a -> Bool) ->
 (x : a) ->
 (xs : Vect k a) ->
 (p x = True) ->
 (Just(FZ,x) = findWithIndex p (x::xs))
  
lemma2 p x xs prf with (p x) proof condition
 | True = Refl
 | False = terrible prf
-------------------------------------------------------------------------------
proof2 :
 (witness : DecEq a) =>
 (p : a -> Bool) ->
 (x : a) ->
 (xs : Vect k a) ->
 (i : Fin (S k)) ->
 (e : a) ->
 (Just(i,e) = findWithIndex p (x::xs)) ->
 (Vect.index i v = e)
  
proof2 @{witness} pt xt xst FZ et prft with (pt xt) proof condition
 | True = ?unproven
 | False = ?unproven
proof2 @{witness} p x xs (FS k) e prf = ?unproven
-------------------------------------------------------------------------------
-- TRUSTED COMPONENTS PART 2/2
-------------------------------------------------------------------------------
-- TRUSTED SPECIFICATION
-------------------------------------------------------------------------------
findWithIndexPreferentiallyFrom :
 DecEq a =>
 (p : a -> Bool) ->
 (begin : Fin n) ->
 (v : Vect n a) ->
 Either
  (find p v = Nothing)
  (DPair
   (Fin n, a) -- data
   (\(i,e) =>   -- specification
    (Vect.index i v = e, -- property 1) element at index.
    ((i' : Fin n) -> -- property 2) index is first valid.
     p (Vect.index i' v) = True ->
     leq2 $ compare (computeSearchIndex begin i) (computeSearchIndex begin i') = True))))

findWithIndexPreferentiallyFrom p begin v with (findWithIndexPreferentiallyFromSimplyTyped p begin v) proof betaReduction
 | Nothing = Left (foo p begin v betaReduction)
 | Just (i,e) = Right ((i,e) ** findWithIndexPreferentiallyFromProof p begin v i e betaReduction)
-------------------------------------------------------------------------------
actualAlive : Maybe FieldedMonster -> Bool

actualAlive Nothing = False
actualAlive (Just monster) with (aliveness $ basic monster)
  | DeadFresh = False
  | DeadStale = False
  | Alive = True
-------------------------------------------------------------------------------
findNextLivingMonster :
 Fin n ->
 Vect n (Maybe FieldedMonster) ->
 Maybe (Fin n)

findNextLivingMonster fin vect =
 findIndexPreferentiallyFrom actualAlive fin vect


 {-
-------------------------------------------------------------------------------
indexMonster : Fin 3 -> Fin 3 -> Player -> Maybe FieldedMonster
indexMonster = ?hole
-------------------------------------------------------------------------------

-- Does not generate any client updates
transformMonster :
 (FieldedMonster -> FieldedMonster) ->
 Fin 3 ->
 Fin 3 ->
 Player ->
 Player
-- do I have the row and column order correct!?!?!
transformMonster mutator row column player =
 case Vect.index column (Vect.index row $ board player) of
  Nothing => ?hole -- absurd error case.
  Just fieldedMonster =>
   let updatedBoard = updateAt column (\column' => updateAt row mutator (index column' $ board player)) (board player) in
   record {board = updatedBoard} player
-------------------------------------------------------------------------------
   -}
