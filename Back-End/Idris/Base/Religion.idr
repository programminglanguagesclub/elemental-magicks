module Base.Religion
import Data.So
import Data.Vect
import Base.Utility
import Base.Bounded
import Base.Preliminaries
import Base.Biology
%access public export
%default total

-- This module contains all of the proof related code
-- which I assert but have not proven yet.
-- Once these are proven, they will stop being part of the religion module.
-- If a theorem consumes approximately a week of work without a solution,
-- it is to be assigned to its own verse from the bible.
-------------------------------------------------------------------------------
isUniqueVect : (n : Nat) -> (v : Vect n (Fin 25)) -> Dec (UniqueVect n v)
isUniqueVect Z [] = Yes UniqueEmpty
isUniqueVect (S k) (x::xs) with (isElem x xs)
  | Yes prfy = ?hole
  | No prfn = ?hole
-------------------------------------------------------------------------------

-- SCIENCE
dog : x = y -> y = x
dog Refl = Refl

------
-- SCIENCE
aaa : (FZ = FS i) -> Void
aaa Refl impossible
---
-----------------

-------------------------------------------------------------------------------
agas :
 (n : Nat) ->
 (UniqueVect (S n) (x::xs) -> Void) ->
 (Elem x xs -> Void) ->
 UniqueVect n xs ->
 Void

agas {xs=xs} {x=x} n notUniqueV notElemX uniqueT =
 notUniqueV (UniqueConcat n xs x notElemX uniqueT)
 
-------------------------------------------------------------------------------
agh : (i = j -> Void) -> (FS i = FS j) -> Void
agh {i=i} {j=j} iNotJ fsIfsJ = iNotJ (FSInjective i j fsIfsJ)



-------------------------------------------------------------------------------
yoo :
 (x : a) ->
 (xs : Vect (S n) a) ->
 (i : Fin (S n)) ->
 (j : Fin (S n)) ->
 Vect.index i xs = Vect.index j xs ->
 Vect.index (FS i) (x::xs) = Vect.index (FS j) (x::xs)

yoo x xs i j vIvJ = vIvJ
-------------------------------------------------------------------------------
littleFindWhere :
 DecEq a =>
 (x : a) ->
 (y : a) ->
 (xs : Vect n a) ->
 (i : Fin n) ->
 (index i xs = x) ->
 (index (FS i) (y::xs) = x)

littleFindWhere x y xs i prf = prf
-------------------------------------------------------------------------------
findWhere :
 DecEq a =>
 (x : a) ->
 (xs : Vect n a) ->
 (Elem x xs) ->
 (i ** index i xs = x)

findWhere x (x::xs) Here = (FZ ** Refl)
findWhere x [] _ impossible
findWhere x (y::xs) (There somewhere) =
 let (i ** prf) = findWhere x xs somewhere in
 ((FS i) ** littleFindWhere x y xs i prf)
-------------------------------------------------------------------------------
aff :
 (n : Nat) ->
 (v : Vect (S n) (Fin 25)) ->
 (UniqueVect (S n) v -> Void) ->
 DPair
  (Fin (S n), Fin (S n))
  (\i => (fst i = snd i -> Void, index (fst i) v = index (snd i) v))

aff Z [x] notUniqueV with (isElem x [])
  | Yes prf impossible
  | No prf = void $ notUniqueV $ UniqueConcat Z [] x prf UniqueEmpty
aff (S n) (x::y::z) notUniqueV with (isElem x (y::z))
  | Yes prf = let (i** p) = findWhere x (y::z) prf in ((FZ, FS i) ** (aaa, dog p))
  | No prf = let ((i1,i2) ** (littleAffProof1, littleAffProof2)) = aff n (y::z) (agas (S n) notUniqueV prf) in ((FS i1,FS i2) ** (agh littleAffProof1, yoo x (y::z) i1 i2 littleAffProof2))
-------------------------------------------------------------------------------
haff : {-DecEq a => {e : a} ->-} e = Vect.index i v -> Elem e v
-- strictly speaking I should not need the decEq typeclass witness.
haff {i=FZ} {v=x::xs} prf = rewrite prf in Here
haff {i=FS fk} {v=x::xs} prf = There $ haff prf {i=fk} {v=xs} 
-------------------------------------------------------------------------------
arglehhh :
 (i = j -> Void) ->
 (j = i) ->
 Void

arglehhh prfNot prf = prfNot $ dog prf
-------------------------------------------------------------------------------
jjjj : i = j -> FS i = FS j
jjjj Refl = Refl
-------------------------------------------------------------------------------
gahaa :
 (FS i = FS j -> Void) ->
 i = j ->
 Void

gahaa prfNot prf = prfNot $ jjjj prf
-------------------------------------------------------------------------------
agagtt :
 (x : a) ->
 (xs : Vect (S n) a) ->
 (Vect.index (FS i) (x::xs)) = (Vect.index (FS j) (x::xs)) ->
 (Vect.index i xs = Vect.index j xs)

agagtt x xs vFivFj = vFivFj
-------------------------------------------------------------------------------
afg :
 (i : Fin (S n)) ->
 (j : Fin (S n)) ->
 (i = j -> Void) ->
 (Vect.index i v = Vect.index j v) ->
 UniqueVect (S n) v ->
 Void

afg FZ FZ iNotJ vIvJ uniqueV = void (iNotJ Refl)

afg (FS fi) FZ iNotJ vIvJ uniqueV = assert_total (afg FZ (FS fi) (arglehhh iNotJ) (dog vIvJ) uniqueV)

afg (FS fi) (FS fj) iNotJ vIvJ UniqueEmpty impossible
afg {n=S n} {v=x::xs} (FS fi) (FS fj) iNotJ vIvJ (UniqueConcat (S n) xs x notElemXXS uniqueXS) =
 afg fi fj (gahaa iNotJ) (agagtt x xs vIvJ) uniqueXS
afg FZ (FS fj) iNotJ vIvJ UniqueEmpty impossible
afg {n=n} {v=x::xs} FZ (FS fj) iNotJ vIvJ (UniqueConcat n xs x notElemXXS uniqueXS) =
 notElemXXS (haff vIvJ)
-------------------------------------------------------------------------------
afh :
 (k : Fin (S (S n))) ->
 (i : Fin (S n)) ->
 (j : Fin (S n)) ->
 (i = j -> Void) ->
 (v : Vect (S (S n)) a) ->
 (Vect.index i (deleteAt k v) = Vect.index j (deleteAt k v)) ->
 DPair
  (Fin (S (S n)), Fin (S (S n)))
  (\i' => (fst i' = snd i' -> Void, Vect.index (fst i') v = Vect.index (snd i') v))

afh {n=n} FZ i j iNotJ (x1::x2::xs) prf = ((FS i,FS j) ** (agh iNotJ, prf))
afh {n=n} (FS fk) FZ FZ iNotJ (x1::x2::xs) prf = void $ iNotJ Refl
afh {n=n} (FS fk) (FS i) FZ iNotJ (x1::x2::xs) prf = ?hole
afh {n=n} (FS fk) FZ (FS j) iNotJ (x1::x2::xs) prf = ?hole
afh {n=S n} (FS fk) (FS i) (FS j) iNotJ (x1::x2::xs) prf =
 let ((i',j')**(prf1,prf2)) = afh fk i j ?hole (x2::xs) ?hole in ?hole
 
-------------------------------------------------------------------------------




