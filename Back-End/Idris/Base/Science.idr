module Base.Religion
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
 UniqueVect (S n) l ->
 UniqueVect n (deleteAt k l)
-- change to explicit arguments...
uniqueRemove {n=S n} l k uniqueL with (isUniqueVect (S n) $ deleteAt k l)
  | Yes prf = prf
  | No prf =
     let ((i, j) ** (iNotJ, vINotvJ)) = aff n (deleteAt k l) prf in
     let ((i', j') ** (i'NotJ',vI'NotvJ')) = afh k i j iNotJ l vINotvJ in
     void (afg i' j' i'NotJ' vI'NotvJ' uniqueL)



 uniqueRemoveHead : (l : Vect (S n) (Fin 25)) -> UniqueVect (S n) l -> UniqueVect n (tail l)
 uniqueRemoveHead {n=S n} (x::xs) (UniqueConcat (S n) xs x uniqueH uniqueT) = uniqueT
 
 deleteAtHeadRemovesHead : (l : Vect (S n) (Fin 25)) -> deleteAt FZ l = tail l
 
 onlyOneEmpty : (v : Vect 0 (Fin 25)) -> v = []
 onlyOneEmpty [] = Refl
 onlyOneEmpty (x::xs) impossible
 
 
 deleteAtHead : (v : Vect (S n) (Fin 25)) -> deleteAt FZ v = tail v
 deleteAtHead [] impossible
 deleteAtHead (x::xs) = Refl
 
 
 deleteInTail : (v : Vect (S (S n)) (Fin 25)) -> (fk : Fin (S n)) -> head (deleteAt (FS fk) v) = head v
 deleteInTail [] i impossible
 deleteInTail (x::xs) i = Refl
 
 
 deleteLemma : (v : Vect (S (S n)) (Fin 25)) -> (fk : Fin (S n)) -> deleteAt (FS fk) v = head v :: (deleteAt fk (tail v))



-----------------------------
--RELIGION


 sdj :
   (n : Nat) ->
   (x : (Fin 25)) ->
   (xs : Vect n (Fin 25)) ->
   (i : Fin (S n)) ->
     -- Not (Elem x xs) ->
     Nat ->
   UniqueVect n xs ->
   UniqueVect (S n) (insertAt i x xs)
 
 uniqueMove :
   (n : Nat) ->
   (l : Vect (S n) (Fin 25)) ->
   (k : Fin (S n)) ->
   (i : Fin (S n)) ->
   UniqueVect (S n) l ->
   UniqueVect (S n) (insertAt i (index k l) (deleteAt k l))
 
 uniqueMove n l k i uniqueL with (isUniqueVect (S n) (insertAt i (index k l) (deleteAt k l)))
   | Yes prf = prf
   | No prf = sdj n (index k l) (deleteAt k l) i Z (uniqueRemove l k uniqueL)
----------------------------------------------------------------
