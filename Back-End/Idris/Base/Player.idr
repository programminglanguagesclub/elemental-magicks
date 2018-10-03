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
import Base.Biology
import Base.Religion
import Base.Science

import Pruviloj.Derive.DecEq


%access public export
%default total


%language ElabReflection

orderingDecEq : (x , y : Ordering) -> Dec (x = y)
%runElab deriveDecEq `{orderingDecEq}

DecEq Ordering where
   decEq x y = orderingDecEq x y
-------------------------------------------------------------------------------

{-
getContainerCardIds : Vect n Card -> Vect n (Fin 25)
getBoardMonsterIds : Vect 9 (Maybe FieldedMonster) -> Vect n (Fin 25)
getBoardMonsterIds _ = []
getSpawnCardId : Maybe Card -> Vect n (Fin 25)
-}

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

-- should add proof that the total number of cards controlled is 25.
 

 timeRemainingMilliseconds : Nat



getThem : Vect n (Maybe a) -> (m ** Vect m a)
getIt : Maybe a -> (m ** Vect m a)



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

















































-- I NEED TO DEFINE UNIQUENESS OF CARDS GIVEN BY EQ, DECEQ or something is given by ID!!!!!!!!!!!!!
-- (OR NOT USE DECEQ)



{- 
 getThem : Vect n (Maybe a) -> (m ** Vect m a)
  getIt : Maybe a -> (m ** Vect m a)
  -}
hgg : x = S n -> x = Z -> Void



data CorrectPlayer : Player -> Type where
  MkCorrectPlayer :
   (player : Player) -> 
   (length25 :
    (fst $ hand player) +
    (fst $ graveyard player) +
    (fst $ banished player) +
    (fst $ getThem $ board player) +
    (fst $ getIt $ spawnCard player) = 25) ->
   UniqueVect
    ((map Base.Card.getId $ snd $ hand player) ++
     (map Base.Card.getId $ snd $ graveyard player) ++
     (map Base.Card.getId $ snd $ banished player) ++
     (map (Base.Objects_basic.BasicFieldedMonster.id . Base.Card.FieldedMonster.basic) $ snd $ getThem $ board player) ++
     (map Base.Card.getId $ snd $ getIt $ spawnCard player)) ->
   CorrectPlayer player 



transplantIndexToEnd :
 (l : Vect (S n) (Fin 25)) ->
 (i : Fin (S n)) ->
 (k : Vect m (Fin 25)) ->
 (q : Vect o (Fin 25)) ->
 (r : Vect a (Fin 25)) ->
 (s : Vect b (Fin 25)) ->
 UniqueVect (l ++ k ++ q ++ r ++ s) ->
 UniqueVect ((Vect.deleteAt i l) ++ (k ++ [Vect.index i l]) ++ q ++ r ++ s)





bhoo :
 (k : Nat) ->
 (playerGraveyardLength : Nat) ->
 (playerBanishedLength : Nat) ->
 (boardLength : Nat) ->
 (spawnLength : Nat) ->
 plus (plus (plus (plus k (plus playerGraveyardLength 1)) playerBanishedLength) boardLength) spawnLength =
 S (plus (plus (plus (plus k playerGraveyardLength) playerBanishedLength) boardLength) spawnLength)

{-
(plus (plus (plus k (plus playerGraveyardLength 1)) playerBanishedLength) (fst (getThem board))) (fst (getIt spawnCard)) = S (plus (plus (plus (plus k playerGraveyardLength) playerBanishedLength) (fst (getThem board))) (fst (getIt spawnCard)))

                       S (plus (plus (plus (plus k playerGraveyardLength) playerBanishedLength) (fst (getThem board))) (fst (getIt spawnCard)))
                                       and
                                                               plus (plus (plus (plus k (plus playerGraveyardLength 1)) playerBanishedLength) (fst (getThem board))) (fst (getIt spawnCard))


-}





{-

 tail (map getId playerHand) ++
 (map getId playerGraveyard ++ [head (map getId playerHand)]) ++
 map getId playerBanished ++ map (id . basic) (snd (getThem board)) ++ map getId (snd (getIt spawnCard))

 
 tail (Data.Vect.Vect n implementation of Prelude.Functor.Functor, method map getId playerHand) ++
 Data.Vect.Vect n implementation of Prelude.Functor.Functor, method map getId (playerGraveyard ++ [head playerHand]) ++
 Data.Vect.Vect n implementation of Prelude.Functor.Functor, method map getId playerBanished ++
 Data.Vect.Vect n implementation of Prelude.Functor.Functor, method map (\x => id (basic x)) (snd (getThem board)) ++
 Data.Vect.Vect n implementation of Prelude.Functor.Functor, method map getId (snd (getIt spawnCard))



                                                                                                                                                                                                                                                                                             -}









yyl : (f : a -> b) -> (i : Fin (S n)) -> (l : Vect (S n) a) -> map f (Vect.deleteAt i l) = Vect.deleteAt i (map f l)
yyl2 : (f : a -> b) -> (i : Fin (S m)) -> (grave : Vect n a) -> (hand : Vect (S m) a) -> map f (grave ++ [Vect.index i hand]) = (map f grave) ++ (map f [Vect.index i hand])
yyl3 : {a : Type} -> {b : Type} -> {m : Nat} -> (f : a -> b) -> (i : Fin (S m)) -> (hand : Vect (S m) a) -> f (Vect.index i hand) = Vect.index i (map f hand)

hu : S k = S m -> k = m

moveCardFromHandToGraveyard :
 CorrectPlayer player ->
 (m : Nat) ->
 (i : Fin (S m)) ->
 ((fst $ hand player) = S m) ->
 (player' ** CorrectPlayer player')

moveCardFromHandToGraveyard
 (MkCorrectPlayer
  (MkPlayer
    board
    rowTarget
    (playerHandLength ** playerHand)
    (playerGraveyardLength ** playerGraveyard)
    (playerBanishedLength ** playerBanished)
    spawnCard
    soulCards
    thoughtsResource
    knowledge
    temporaryId
    timeRemainingMilliseconds)
  length25
  uniqueVect)
 m i prf with (playerHandLength) proof p
  | Z = void $ hgg prf Refl
  | S k =
   let player' = (MkPlayer
                   board
                   rowTarget
       --            (k ** deleteAt (let x = the (k=m) (hu prf) in rewrite x in i) playerHand)a
                   (m ** (deleteAt i (believe_me playerHand)))
                   ((playerGraveyardLength + 1) ** ((playerGraveyard) ++ [Data.Vect.index i (let x = the (m=k) (hu $ sym prf) in rewrite x in playerHand)]))
                   (playerBanishedLength ** playerBanished)
                   spawnCard
                   soulCards
                   thoughtsResource
                   knowledge
                   temporaryId
                   timeRemainingMilliseconds) in
   let uniqueTransplant = transplantIndexToEnd
                           (map Base.Card.getId $ playerHand)
                           (rewrite hu prf in i)
                           (map Base.Card.getId $ playerGraveyard)
                           (map Base.Card.getId $ playerBanished)
                           (map (Base.Objects_basic.BasicFieldedMonster.id . Base.Card.FieldedMonster.basic) $ snd $ getThem $ board)
                           (map Base.Card.getId $ snd $ getIt $ spawnCard)
                           uniqueVect in
   ?hole
                   {-        
   (player' ** (MkCorrectPlayer
                 player'
                 (rewrite bhoo k playerGraveyardLength playerBanishedLength (fst (getThem board)) (fst (getIt spawnCard)) in length25)
                 
                 
                 ({-rewrite yyl getId i playerHand in-} {-rewrite yyl2 getId i playerGraveyard playerHand in-} {-rewrite yyl3 getId i playerHand in-}
                  transplantIndexToEnd
                   (map Base.Card.getId $ playerHand)
                   (rewrite hu prf in i)
                   (map Base.Card.getId $ playerGraveyard)
                   (map Base.Card.getId $ playerBanished)
                   (map (Base.Objects_basic.BasicFieldedMonster.id . Base.Card.FieldedMonster.basic) $ snd $ getThem $ board)
                   (map Base.Card.getId $ snd $ getIt $ spawnCard)
                   uniqueVect)))
                   -}



{-
yyl : (f : a -> b) -> (l : Vect (S n) a) -> map f (tail l) = tail (map f l)
yyl2 : (f : a -> b) -> (grave : Vect n a) -> (hand : Vect (S m) a) -> map f (grave ++ [head hand]) = (map f grave) ++ (map f [head hand])
yyl3 : {a : Type} -> {b : Type} -> {m : Nat} -> (f : a -> b) -> (hand : Vect (S m) a) -> f (head hand) = head (map f hand)

moveCardFromHandToGraveyard :
 CorrectPlayer player ->
 (m : Nat) ->
 (i : Fin (S m)) ->
 ((fst $ hand player) = S m) ->
 (player' ** CorrectPlayer player')

moveCardFromHandToGraveyard
 (MkCorrectPlayer
  (MkPlayer
    board
    rowTarget
    (playerHandLength ** playerHand)
    (playerGraveyardLength ** playerGraveyard)
    (playerBanishedLength ** playerBanished)
    spawnCard
    soulCards
    thoughtsResource
    knowledge
    temporaryId
    timeRemainingMilliseconds)
  length25
  uniqueVect)
 m i prf with (playerHandLength)
  | Z = void $ hgg prf Refl
  | S k =
   let player' = (MkPlayer
                   board
                   rowTarget
                   (k ** tail playerHand)
                   ((playerGraveyardLength + 1) ** ((playerGraveyard) ++ [Data.Vect.head playerHand]))
                   (playerBanishedLength ** playerBanished)
                   spawnCard
                   soulCards
                   thoughtsResource
                   knowledge
                   temporaryId
                   timeRemainingMilliseconds) in
   let uniqueTransplant = transplantStartToEnd
                           (map Base.Card.getId $ playerHand) --playerHand playerGraveyard playerBanished uniqueVect in
                           (map Base.Card.getId $ playerGraveyard)
                           (map Base.Card.getId $ playerBanished)
                           (map (Base.Objects_basic.BasicFieldedMonster.id . Base.Card.FieldedMonster.basic) $ snd $ getThem $ board)
                           (map Base.Card.getId $ snd $ getIt $ spawnCard)
                           uniqueVect in
   (player' ** (MkCorrectPlayer
                 player'
                 (rewrite bhoo k playerGraveyardLength playerBanishedLength (fst (getThem board)) (fst (getIt spawnCard)) in length25)
                 (rewrite yyl getId playerHand in rewrite yyl2 getId playerGraveyard playerHand in rewrite yyl3 getId playerHand in
                  transplantStartToEnd
                  (map Base.Card.getId $ playerHand)
                  (map Base.Card.getId $ playerGraveyard)
                  (map Base.Card.getId $ playerBanished)
                  (map (Base.Objects_basic.BasicFieldedMonster.id . Base.Card.FieldedMonster.basic) $ snd $ getThem $ board)
                  (map Base.Card.getId $ snd $ getIt $ spawnCard)
                  uniqueVect)))



-}

































{-


-------------------------------------------------------------------------------
data CorrectPlayer : Player -> Type where
  MkCorrectPlayer :
   (player : Player) -> 
   (length25 : (fst $ hand player) + ((fst $ graveyard player) + (fst $ banished player)) = 25) ->
   UniqueVect ({-map Base.Card.getId $-} (snd $ hand player) ++ (snd $ graveyard player) ++ (snd $ banished player)) ->
  CorrectPlayer player                                                                                  
-------------------------------------------------------------------------------
hgg : x = S n -> x = Z -> Void
--hgg {x=Z} _ _ impossible
--hgg {x=S n} _ _ impossible


faf : (x : Nat) -> (y : Nat) -> plus (S k) (plus x y) = 25 -> plus k (S (plus x y)) = 25
faf x y prf = ?hole


transplantStartToEnd :
 (l : Vect (S n) Card) ->
 (k : Vect m Card) ->
 (q : Vect o Card) ->
 UniqueVect (l ++ k ++ q) ->
 UniqueVect ((Vect.tail l) ++ (k ++ [Vect.head l]) ++ q)

how : 
 (k : Nat) ->
 (playerGraveyardLength : Nat) ->
 (playerBanishedLength : Nat) ->
 plus k (plus (plus playerGraveyardLength 1) playerBanishedLength) = plus k (S (plus playerGraveyardLength playerBanishedLength))

hoh :
 (k : Nat) ->
 (playerGraveyardLength : Nat) ->
 (playerBanishedLength : Nat) ->
 S (plus k (plus playerGraveyardLength playerBanishedLength)) = 25 ->
 --plus k (plus (plus playerGraveyardLength 1) playerBanishedLength) = 25
 plus (S k) (plus playerGraveyardLength playerBanishedLength) = 25

moveCardFromHandToGraveyard :
 CorrectPlayer player ->
 (m : Nat) ->
 (i : Fin (S m)) ->
 ((fst $ hand player) = S m) ->
 (player' ** CorrectPlayer player')

moveCardFromHandToGraveyard
 (MkCorrectPlayer
  (MkPlayer
    board
    rowTarget
    (playerHandLength ** playerHand)
    (playerGraveyardLength ** playerGraveyard)
    (playerBanishedLength ** playerBanished)
    spawnCard
    soulCards
    thoughtsResource
    knowledge
    temporaryId
    timeRemainingMilliseconds)
  length25
  uniqueVect)
 m i prf with (playerHandLength) --proof p
  | Z = void $ hgg prf Refl
  | S k =
     let player' =
       (MkPlayer board rowTarget (k ** tail playerHand) ((playerGraveyardLength + 1) ** ({-rewrite plusCommutative 1 playerGraveyardLength in-} (playerGraveyard) ++ [Data.Vect.head playerHand])) (playerBanishedLength ** playerBanished) spawnCard soulCards thoughtsResource knowledge temporaryId timeRemainingMilliseconds)
     in
     --let q = the (playerHandLength + (playerGraveyardLength + playerBanishedLength) = 25) {-(rewrite plusCommutative k 1 in length25)-} Refl in
     --let r = the q Refl in
   --let p = ((fst $ hand player') + ((fst $ graveyard player') + (fst $ banished player')) = 25) in
   --let p1 = the p ?hole in
  
  
     --let playerHand'Unique = ?hole in
    -- let playerGraveyard'Unique = ?hole in
    -- let playerBanished'Unique = ?hole in
    -- let allUnique = uniqueConcat playerHand'Unique playerGraveyard'Unique playerBanished'Unique 

 --    let p1 = the ((rewrite plusCommutative 1 playerGraveyardLength in playerGraveyard ++ [head playerHand]) = snd (graveyard player'))
   --               Refl in
     let uniqueTransplant = transplantStartToEnd playerHand playerGraveyard playerBanished uniqueVect in

     (player' ** (MkCorrectPlayer player' (rewrite how k playerGraveyardLength playerBanishedLength in faf playerGraveyardLength playerBanishedLength (hoh k playerGraveyardLength playerBanishedLength length25)) uniqueTransplant))
-------------------------------------------------------------------------------


-}

































































































{-

                UniqueVect (tail playerHand ++ (playerGraveyard ++ [head playerHand]) ++ playerBanished) (Type of uniqueTransplant)
                        and
                                        UniqueVect (snd (hand player') ++ snd (graveyard player') ++ snd (banished player')) (Expected type)



 transplantStartToEnd :
   (l : Vect (S n) (Fin 25)) ->
     (k : Vect m (Fin 25)) ->
       UniqueVect (l ++ k) ->
         UniqueVect ((Vect.tail l) ++ (k ++ [Vect.head l]))



uniqueConcat4 :
 (l : Vect n (Fin 25)) ->
  (k : Vect m (Fin 25)) ->
   (q : Vect o (Fin 25)) ->
    UniqueVect ((q ++ l) ++ k) ->
     UniqueVect l


              Type mismatch between
                                      25
                                                      and
                                                                              plus k (S (plus playerGraveyardLength playerBanishedLength))



-}






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
terrible Refl impossible
-------------------------------------------------------------------------------
apaap : {-((b = True) -> Void)-} if False then t else f = f
apaap = Refl

agaag : (b : Bool) -> (q : a) -> (r : a) -> ((b = True) -> Void) -> (if b then q else r) = r
agaag b q r prf =
  case b of
    False => Refl
    True => void (prf Refl)
-------------------------------------------------------------------------------
helper :
 (p : a -> Bool) ->
 (x : a) ->
 ((p x = True) -> Void) ->
 ((if p x then Just x else Nothing) = Nothing)

helper p x pNotX = agaag (p x) (Just x) Nothing pNotX
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
