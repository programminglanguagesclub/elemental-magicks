module Base.Science
import Data.So
import Data.Vect
import Base.Utility
import Base.Bounded
import Base.Preliminaries
import Base.Biology
import Base.Religion
%access public export
%default total

-- This module contains the code related to the dependent type work for this project.
-- Here you will find many proofs.
-- This module is populated as open problems from the religion module are solved.
-------------------------------------------------------------------------------
uniqueRemove :
 (l : Vect (S n) (Fin 25)) ->
 (k : Fin (S n)) ->
 UniqueVect l ->
 UniqueVect (deleteAt k l)

uniqueRemove {n=S n} l k uniqueL with (isUniqueVect (S n) $ deleteAt k l)
  | Yes prf = prf
  | No prf =
     let ((i, j) ** (iNotJ, vINotvJ)) = findRepeatWitness n (deleteAt k l) prf in
     let ((i', j') ** (i'NotJ',vI'NotvJ')) = afh k i j iNotJ l vINotvJ in
     void (notUniqueFromEqualAnywhere i' j' i'NotJ' vI'NotvJ' uniqueL)
uniqueRemove {n= Z} [x] FZ (UniqueConcat [] x _ UniqueEmpty) = UniqueEmpty
-------------------------------------------------------------------------------
uniqueRemoveHead :
 (l : Vect (S n) (Fin 25)) ->
 UniqueVect l ->
 UniqueVect (tail l)
uniqueRemoveHead (x::xs) (UniqueConcat xs x uniqueH uniqueT) = uniqueT
uniqueRemoveHead [] _ impossible
-------------------------------------------------------------------------------
deleteAtHeadRemovesHead : (l : Vect (S n) (Fin 25)) -> deleteAt FZ l = tail l
deleteAtHeadRemovesHead (x::xs) = Refl
-------------------------------------------------------------------------------
onlyOneEmpty :
 (v : Vect 0 (Fin 25)) ->
 v = []

onlyOneEmpty [] = Refl
onlyOneEmpty (x::xs) impossible
-------------------------------------------------------------------------------
deleteAtHead :
 (v : Vect (S n) (Fin 25)) ->
 deleteAt FZ v = tail v

deleteAtHead [] impossible
deleteAtHead (x::xs) = Refl
-------------------------------------------------------------------------------
deleteInTail :
 (v : Vect (S (S n)) (Fin 25)) ->
 (fk : Fin (S n)) ->
 head (deleteAt (FS fk) v) = head v

deleteInTail [] i impossible
deleteInTail (x::xs) i = Refl
-------------------------------------------------------------------------- 
deleteLemma : (v : Vect (S (S n)) (Fin 25)) -> (fk : Fin (S n)) -> deleteAt (FS fk) v = head v :: (deleteAt fk (tail v))
deleteLemma (x::xs) fk = Refl

--RELIGION

huh : (x : (Fin 25)) -> (x1 : (Fin 25)) -> (xs : Vect n (Fin 25)) -> Not (Elem x (x1::xs)) -> Not (Elem x xs)
hhhl : (x : Fin 25) -> (xs : Vect n (Fin 25)) -> (x1 : Fin 25) -> (i : Fin (S n)) -> (insertAt (FS i) x (x1::xs)) = x1 :: (insertAt i x xs)

sdj :
   (n : Nat) ->
   (x : (Fin 25)) ->
   (xs : Vect n (Fin 25)) ->
   (i : Fin (S n)) ->
   Not (Elem x xs) ->
   UniqueVect xs ->
   UniqueVect (insertAt i x xs)
{-
sdj n x xs FZ prf uniqueXS = UniqueConcat xs x prf uniqueXS
sdj (S k) x (x1::xs) (FS i) prf (UniqueConcat xs x1 prf' uniqueXS) =
  let y = sdj k x xs i ?hole uniqueXS in
  let (q::qs) = insertAt i x xs in
  UniqueConcat (q::qs) x1 (the (Not (Elem x1 (q::qs))) (huh x1 q qs ?hole)) y
  -}

{-(UniqueConcat xs x1 prf' uniqueXS) with (isElem x1 (insertAt i x xs))
  | Yes p = ?hole
  | No p = ?hole -- UniqueConcat (insertAt i x xs) x1 p (sdj k x xs i (huh x x1 xs prf') uniqueXS)
-}
  ---(sdj k _ _ _ _ _)

--sdj (S (S k)) x (x1::x2::xs) (FS (FS i)) prf (UniqueConcat xs x1 prf' uniqueXS) with (isElem x1 (insertAt i x xs))
-- | Yes p = ?hole
-- | No p = UniqueConcat (insertAt i x xs) x1 p (sdj k _ _ _ _ _)



uniqueMove :
   (n : Nat) ->
   (l : Vect (S n) (Fin 25)) ->
   (k : Fin (S n)) ->
   (i : Fin (S n)) ->
   UniqueVect l ->
   UniqueVect (insertAt i (index k l) (deleteAt k l))
 
uniqueMove n l k i uniqueL with (isUniqueVect (S n) (insertAt i (index k l) (deleteAt k l)))
   | Yes prf = prf
   | No prf = sdj n (index k l) (deleteAt k l) i ?hole (uniqueRemove l k uniqueL)
----------------------------------------------------------------
