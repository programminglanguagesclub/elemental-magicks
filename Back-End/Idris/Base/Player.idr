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

-------------------------------------------------------------------------------
record Player where
 constructor MkPlayer
 board : Vect 3 (Vect 3 (Maybe Monster)) -- should wrap this in a datatype, and/or use matrix..
 rowTarget : Vect 3 (Fin 3)
 hand : List Card
 graveyard : List Card
 discard : List Card
 spawnCard : Maybe Card
 soulCards : Vect 5 Monster
 thoughtsResource : Bounded 0 Preliminaries.absoluteUpperBound
 knowledge : Vect 6 (Bounded 0 9)
 temporaryId : String
-- should add proof that the total number of cards controlled is 30.

record DrawPlayer where
 constructor MkDrawPlayer
 hand : BoundedList 25 Card -- might want to make this a normal list.
 soulCards : Vect 5 (Maybe Monster)
 temporaryId : String
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
emptyDiscard : List Card
emptyDiscard = []
-------------------------------------------------------------------------------
emptyGraveyard : List Card
emptyGraveyard = []
-------------------------------------------------------------------------------
emptyBoard : Vect 3 (Vect 3 (Maybe Monster))
emptyBoard = replicate 3 $ replicate 3 Nothing
-------------------------------------------------------------------------------
emptySoul : Vect 5 (Maybe Monster)
emptySoul = replicate 5 Nothing
-------------------------------------------------------------------------------
emptySpawn : Maybe Card
emptySpawn = Nothing
-------------------------------------------------------------------------------
newPlayer :
 String ->
 Vect 5 Monster ->
 Vect 30 Card ->
 Player

newPlayer playerId soul hand =
 MkPlayer
  emptyBoard
  initialRowTarget
  (toList hand)
  emptyGraveyard
  emptyDiscard
  emptySpawn
  soul
  initialThoughts
  initialKnowledge
  playerId
-------------------------------------------------------------------------------
newDrawPlayer : String -> DrawPlayer
newDrawPlayer playerId = MkDrawPlayer [] emptySoul playerId
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
 Vect 5 (Maybe Monster) ->
 Vect 5 (Maybe Monster) ->
 BoundedList 25 Card ->
 BoundedList 25 Card ->
 BoundedList 60 Card

buildDrawnCardsList v1 v2 l1 l2 = getAll v1 v2 l1 l2 (\x => MonsterCard x)
-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------
flattenBoard : Vect 3 (Vect 3 (Maybe Monster)) -> Vect 9 (Maybe Monster)
flattenBoard [row0, row1, row2] = row0 ++ row1 ++ row2
-------------------------------------------------------------------------------
unflattenBoard : Vect 9 (Maybe Monster) -> Vect 3 (Vect 3 (Maybe Monster))
unflattenBoard [p0,p1,p2,p3,p4,p5,p6,p7,p8] =
 [[p0,p1,p2],[p3,p4,p5],[p6,p7,p8]]
-------------------------------------------------------------------------------
idMatches :
 Nat ->
 Maybe Monster ->
 Bool

idMatches monsterId Nothing = False
idMatches monsterId (Just monster) = (id $ basic monster) == monsterId
-------------------------------------------------------------------------------
findBoardMonsterIndex :
 Nat ->
 Vect 3 (Vect 3 (Maybe Monster)) ->
 Maybe (Fin 9)

findBoardMonsterIndex monsterId board =
 findIndex (idMatches monsterId) (flattenBoard board)
-------------------------------------------------------------------------------
findBoardMonster :
 Nat ->
 Vect 3 (Vect 3 (Maybe Monster)) ->
 Maybe Monster

findBoardMonster monsterId board =
 case findBoardMonsterIndex monsterId board of
  Nothing => Nothing
  Just monsterIndex => Vect.index monsterIndex (flattenBoard board)
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
-- correct the ordering

computeSearchIndex : (begin : Fin n) -> (i : Fin n) -> Fin n
computeSearchIndex FZ i = i
computeSearchIndex (FS k1) (FS k2) = computeSearchIndex (weaken k1) (weaken k2)
computeSearchIndex (FS k) FZ = computeSearchIndex (weaken k) last
-------------------------------------------------------------------------------
leq2 : Ordering -> Bool
leq2 GT = False
leq2 EQ = True
leq2 LT = True
-------------------------------------------------------------------------------
zeroIndexIsFirst : (x : Fin (S k)) -> leq2 $ compare FZ x = True
zeroIndexIsFirst FZ = Refl
zeroIndexIsFirst (FS k) = Refl
-------------------------------------------------------------------------------
nowTheProof : (i2 : Fin (S k)) -> leq2 $ compare (computeSearchIndex FZ FZ) (computeSearchIndex FZ i2) = True
nowTheProof {k=k} i2 with (computeSearchIndex FZ i2)
 | FZ with (leq2 $ compare (computeSearchIndex (the (Fin (S k)) FZ) (the (Fin (S k)) FZ)) (the (Fin (S k)) FZ))
  | True = Refl
 | FS fk with (leq2 $ compare (computeSearchIndex (the (Fin (S k)) FZ) (the (Fin (S k)) FZ)) (FS fk))
  | True = Refl
-------------------------------------------------------------------------------
helper : (p : a -> Bool) -> (x : a) -> ((p x = True) -> Void) -> ((if p x then Just x else Nothing) = Nothing)
helper = ?hole
-------------------------------------------------------------------------------

mutual -- do I need these to be mutual if the codependency involves types, not just terms?
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
-- scrubbed version
  findWithIndex : DecEq a => (a -> Bool) -> (v : Vect n a) -> Maybe (Fin n, a)
  findWithIndex p [] = Nothing
  findWithIndex p (x::xs) with (p x)
    | True = Just (FZ, x)
    | False = map ugh $ findWithIndex p xs
-------------------------------------------------------------------------------
  findWithIndexFrom : DecEq a => (a -> Bool) -> Fin n -> (v1 : Vect n a) -> Maybe (Fin n, a)
  findWithIndexFrom p FZ [x] = if p x then Just (FZ, x) else Nothing
  findWithIndexFrom p (FS k) (x :: xs) = (\(i,e) => (FS i, e)) <$> findWithIndexFrom p k xs
-------------------------------------------------------------------------------
  -- THING TO PROVE #1
  foo : DecEq a => (p : a -> Bool) -> (begin : Fin n) -> (v : Vect n a) -> Nothing = findWithIndexPreferentiallyFromSimplyTyped p begin v -> find p v = Nothing
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
-------------------------------------------------------------------------------
-- THING TO PROVE #2
  findWithIndexPreferentiallyFromProof p begin v i e prf =
    let prf1 = findWithIndexPreferentiallyFromProof1 p begin v i e prf in
    let prf2 = (\i' => \prf2' => findWithIndexPreferentiallyFromProof2 p begin v i e prf prf1 i' prf2') in
    (prf1, prf2)
-------------------------------------------------------------------------------
  
  terrible : False = True -> a

  ugh : (Fin k, a) -> (Fin (S k), a)
  ugh (i,e) = (FS i, e)

  proof1 :
    DecEq a =>
    (p : a -> Bool) ->
    (v : Vect (S k) a) ->
    (i : Fin (S k)) ->
    (e : a) ->
    (findWithIndexPreferentiallyFromSimplyTyped p FZ v = findWithIndex p v)
  proof1 p v i e = Refl


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

-- False = p x (Type of condition)
  flip : a = b -> b = a

{-

   proof2 :
        (witness : DecEq a) =>
                        (p : a -> Bool) ->
                             (x : a) ->
                                  (xs : Vect k a) ->
                                       (i : Fin (S k)) ->
                                            (e : a) ->
                                                 (Just(i,e) = findWithIndex p (x::xs)) ->
                                                                   (Just(i,e) = (if p x then Just (FZ, x) else map ugh $ findWithIndex @{witness} {a=a} {n=k}p xs))
                                                                    
                                                                      {-(Vect.index i v = e)-}
                                                                       
                                                                          proof2 @{witness} p x xs FZ e prf with (p x) proof condition
                                                                               | True = rewrite prf in Refl --rewrite lemma2 @{witness} p x xs (flip condition) in prf
                                                                                    | False = ?hole -- Refl
                                                                                     
                                                                                     -}


  proof2 :
    (witness : DecEq a) =>
    (p : a -> Bool) ->
    (x : a) ->
    (xs : Vect k a) ->
    (i : Fin (S k)) ->
    (e : a) ->
    (Just(i,e) = findWithIndex p (x::xs)) ->
    (Just(i,e) = (if p x then Just (FZ, x) else map ugh $ findWithIndex {-@{witness} {a=a} {n=k}-}p xs))

 {-(Vect.index i v = e)-}

  proof2 @{witness} pt xt xst FZ et prft with (pt xt) proof condition
    | True = rewrite prft in Refl --rewrite lemma2 @{witness} p x xs (flip condition) in prf
    | False = {-rewrite prft in-} Refl -- Refl
   
   {-

   Type mismatch between
           Just (FZ, x) = Just (FZ, x) (Type of Refl)
           and
                   Just (FZ, e) = Just (FZ, x) (Expected type)
                   -}

{-
findWithIndex : DecEq a => (a -> Bool) -> (v : Vect n a) -> Maybe (Fin n, a)
findWithIndex p [] = Nothing
findWithIndex p (x::xs) = if p x then Just (FZ, x) else map (\(i,e) => (FS i,e)) $ findWithIndex p xs
-}

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

  findWithIndexPreferentiallyFromProof1 p FZ v i e prf = ?hole
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
-------------------------------------------------------------------------------
-- END MUTUAL
-------------------------------------------------------------------------------
actualAlive : Maybe Monster -> Bool

actualAlive Nothing = False
actualAlive (Just monster) with (aliveness $ basic monster)
  | DeadFresh = False
  | DeadStale = False
  | Alive = True
-------------------------------------------------------------------------------
findNextLivingMonster :
 Fin n ->
 Vect n (Maybe Monster) ->
 Maybe (Fin n)

findNextLivingMonster fin vect =
 findIndexPreferentiallyFrom actualAlive fin vect
-------------------------------------------------------------------------------
indexMonster : Fin 3 -> Fin 3 -> Player -> Maybe Monster
indexMonster = ?hole
-------------------------------------------------------------------------------

-- Does not generate any client updates
transformMonster :
 (Monster -> Monster) -> Fin 3 ->
 Fin 3 ->
 Player ->
 Player

transformMonster mutator row column player = ?hole
-------------------------------------------------------------------------------
