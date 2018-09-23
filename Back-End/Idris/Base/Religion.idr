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
{-


data UniqueVect : {n : Nat} -> Vect n (Fin 25) -> Type where
   UniqueEmpty : UniqueVect []
    UniqueConcat :
      (xs : Vect n (Fin 25)) ->
        (x : Fin 25) ->
          Not (Elem x xs) ->
            UniqueVect xs ->
              UniqueVect (x :: xs)

              -}


isUniqueVect : (n : Nat) -> (v : Vect n (Fin 25)) -> Dec (UniqueVect v)
isUniqueVect Z [] = Yes UniqueEmpty
isUniqueVect (S k) (x::xs) with (isElem x xs)
  | Yes prfy = ?hole
  | No prfn = ?hole
-------------------------------------------------------------------------------

-- SCIENCE
equalityCommutative : x = y -> y = x
equalityCommutative Refl = Refl
-------------------------------------------------------------------------------
headOkayThenTailNotOkay :
 (UniqueVect (x::xs) -> Void) ->
 (Elem x xs -> Void) ->
 UniqueVect xs ->
 Void

headOkayThenTailNotOkay {xs=xs} {x=x} notUniqueV notElemX uniqueT =
 notUniqueV (UniqueConcat xs x notElemX uniqueT)
 
-------------------------------------------------------------------------------
FSInequality : (i = j -> Void) -> (FS i = FS j) -> Void
FSInequality {i=i} {j=j} iNotJ fsIfsJ = iNotJ (FSInjective i j fsIfsJ)
-------------------------------------------------------------------------------
reindexAppend :
 (x : a) ->
 (xs : Vect (S n) a) ->
 (i : Fin (S n)) ->
 (j : Fin (S n)) ->
 Vect.index i xs = Vect.index j xs ->
 Vect.index (FS i) (x::xs) = Vect.index (FS j) (x::xs)

reindexAppend x xs i j vIvJ = vIvJ
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
findRepeatWitness :
 (n : Nat) ->
 (v : Vect (S n) (Fin 25)) ->
 (UniqueVect v -> Void) ->
 DPair
  (Fin (S n), Fin (S n))
  (\i => (fst i = snd i -> Void, index (fst i) v = index (snd i) v))

findRepeatWitness Z [x] notUniqueV with (isElem x [])
  | Yes prf impossible
  | No prf = void $ notUniqueV $ UniqueConcat [] x prf UniqueEmpty
findRepeatWitness (S n) (x::y::z) notUniqueV with (isElem x (y::z))
  | Yes prf = let (i** p) = findWhere x (y::z) prf in ((FZ, FS i) ** (FZNotFS, equalityCommutative p))
  | No prf =
   let ((i1,i2) ** (littleAffProof1, littleAffProof2)) = findRepeatWitness n (y::z) (headOkayThenTailNotOkay notUniqueV prf) in
   ((FS i1,FS i2) ** (FSInequality littleAffProof1, reindexAppend x (y::z) i1 i2 littleAffProof2))
-------------------------------------------------------------------------------
elemFromFound : e = Vect.index i v -> Elem e v
elemFromFound {i=FZ} {v=x::xs} prf = rewrite prf in Here
elemFromFound {i=FS fk} {v=x::xs} prf = There $ elemFromFound prf {i=fk} {v=xs} 
-------------------------------------------------------------------------------
inequalityCommutative :
 (i = j -> Void) ->
 (j = i) ->
 Void

inequalityCommutative prfNot prf = prfNot $ equalityCommutative prf
-------------------------------------------------------------------------------
fSEq : i = j -> FS i = FS j
fSEq Refl = Refl
-------------------------------------------------------------------------------
fSNotEq :
 (FS i = FS j -> Void) ->
 i = j ->
 Void

fSNotEq prfNot prf = prfNot $ fSEq prf
-------------------------------------------------------------------------------
reindexUnappend :
 (x : a) ->
 (xs : Vect (S n) a) ->
 (Vect.index (FS i) (x::xs)) = (Vect.index (FS j) (x::xs)) ->
 (Vect.index i xs = Vect.index j xs)

reindexUnappend x xs vFivFj = vFivFj
-------------------------------------------------------------------------------
notUniqueFromEqualAnywhere :
 (i : Fin (S n)) ->
 (j : Fin (S n)) ->
 (i = j -> Void) ->
 {v : Vect (S n) (Fin 25)} -> -- (not S $ S n???)
 (Vect.index i v = Vect.index j v) ->
 UniqueVect v ->
 Void

notUniqueFromEqualAnywhere FZ FZ iNotJ vIvJ uniqueV = void (iNotJ Refl)

notUniqueFromEqualAnywhere (FS fi) FZ iNotJ vIvJ uniqueV = assert_total (notUniqueFromEqualAnywhere FZ (FS fi) (inequalityCommutative iNotJ) (equalityCommutative vIvJ) uniqueV)

notUniqueFromEqualAnywhere (FS fi) (FS fj) iNotJ vIvJ UniqueEmpty impossible
notUniqueFromEqualAnywhere {n=S n} {v=x::xs} (FS fi) (FS fj) iNotJ vIvJ (UniqueConcat xs x notElemXXS uniqueXS) =
 notUniqueFromEqualAnywhere fi fj (fSNotEq iNotJ) (reindexUnappend x xs vIvJ) uniqueXS
notUniqueFromEqualAnywhere FZ (FS fj) iNotJ vIvJ UniqueEmpty impossible
notUniqueFromEqualAnywhere {n=n} {v=x::xs} FZ (FS fj) iNotJ vIvJ (UniqueConcat xs x notElemXXS uniqueXS) =
 notElemXXS (elemFromFound vIvJ)
-------------------------------------------------------------------------------
aagf : (i = j -> Void) -> FS i = FS j -> Void

yuwa :
 (k : Fin (S (S n))) ->
 (i : Fin n) ->
 (v : Vect (S (S n)) a) ->
 (x : a) ->
 (Vect.index (FS i) (deleteAt k v) = x) ->
 DPair
  (Fin (S (S n)))
  (\i' => Vect.index i' v = x)

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

afh {n=n} FZ i j iNotJ (x1::x2::xs) prf = ((FS i,FS j) ** (FSInequality iNotJ, prf))
afh {n=n} (FS fk) FZ FZ iNotJ (x1::x2::xs) prf = void $ iNotJ Refl
afh {n=n} (FS fk) (FS i) FZ iNotJ (x1::x2::xs) prf = ?hole
afh {n=n} (FS fk) FZ (FS j) iNotJ (x1::x2::xs) prf = ?hole
afh {n=S n} (FS fk) (FS i) (FS j) iNotJ (x1::x2::xs) prf =
 let ((i',j')**(prf1,prf2)) = afh fk i j (fSNotEq iNotJ) (x2::xs) prf in
 ((FS i',FS j')**(aagf prf1,reindexAppend x1 (x2::xs) i' j' prf2))
 
-------------------------------------------------------------------------------
gkj : (x : Fin 25) -> (v : Vect n (Fin 25)) -> (w : Vect m (Fin 25)) -> find (==x) (v++w) = Nothing -> find (==x) v = Nothing
 

-- this worked... then just suddenly stopped working...!!
{-
uniqueConcat2 : (l : Vect n (Fin 25)) -> (k : Vect m (Fin 25)) -> UniqueVect (l ++ k) -> UniqueVect l
uniqueConcat2 l [] lkUnique = rewrite gh l in lkUnique -----rewrite (g $ appendNilRightNeutral l) in (rewrite (gh l) in lkUnique) -- put {n=n}{m=m} into scope?
-}
-------------------------------------------------------------------------------
vectNilLeftNeutral :
 (l : Vect n (Fin 25)) ->
 UniqueVect l = UniqueVect ([] ++ l)

vectNilLeftNeutral l = Refl
-------------------------------------------------------------------------------
uniqueConcat :
 (n : Nat) ->
 (m : Nat) ->
 (l : Vect n (Fin 25)) ->
 (k : Vect m (Fin 25)) ->
 UniqueVect (k ++ l) ->
 UniqueVect l

uniqueConcat n Z l [] klUnique = rewrite vectNilLeftNeutral l in klUnique
uniqueConcat n (S m) l (kh::kt) klUnique with (toIndexed (S (plus m n)) ((kh::kt)++l) klUnique)
 | IndexedUniqueEmpty impossible
 | IndexedUniqueConcat (m+n) (kt ++ l) kh uniqueListH uniqueListT =
    uniqueConcat n m l kt (fromIndexed (m+n) (kt ++ l) uniqueListT)
-------------------------------------------------------------------------------
notUniqueConcat :
 (l : Vect n (Fin 25)) ->
 (k : Vect m (Fin 25)) ->
 (UniqueVect l -> Void) ->
 UniqueVect (k++l) ->
 Void

notUniqueConcat {n=n} {m=m} l k notUniqueL uniqueKL =
 notUniqueL $ uniqueConcat n m l k uniqueKL
-------------------------------------------------------------------------------

uniqueConcat2 : (l : Vect n (Fin 25)) -> (k : Vect m (Fin 25)) -> UniqueVect (l ++ k) -> UniqueVect l
uniqueConcat2 [] k _ = UniqueEmpty
uniqueConcat2 {n=S n} {m=m} (lh::lt) k lkUnique with (lkUnique)
  | UniqueEmpty impossible
  | UniqueConcat (lt ++ k) lh headUnique tailUnique =
   let uniqueLTail = uniqueConcat2 lt k tailUnique in
   UniqueConcat lt lh ?hole {-(gkj lh lt k headUnique)-} uniqueLTail

booo : (x : (Fin 25)) -> (xs : Vect n (Fin 25)) -> (ys : Vect n (Fin 25)) -> xs = ys -> (x::xs) = (x::ys)
booo _ _ _ Refl = Refl


aaaat :
 (xLen : Nat) ->
 (yLen : Nat) ->
 (zLen : Nat) ->
 plus xLen (plus yLen zLen) = plus (plus xLen yLen) zLen

-------------------------------------------------------------------------------
vectAppendAssociative' :
 {xLen : Nat} ->
 {yLen : Nat} ->
 {zLen : Nat} ->
 (xs : Vect xLen elem) ->
 (ys : Vect yLen elem) ->
 (zs : Vect zLen elem) ->
 (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

vectAppendAssociative' {xLen=xLen} {yLen=yLen} {zLen=zLen} xs ys zs = ?hole
--(rewrite aaaat xLen yLen zLen in equalityCommutative (vectAppendAssociative'' xs ys zs))

-------------------------------------------------------------------------------
plusAssociative' : (left : Nat) -> (centre : Nat) -> (right : Nat) ->
                    (left + centre) + right = left + (centre + right)
-------------------------------------------------------------------------------
hak : (q : Vect a (Fin 25)) -> (l : Vect b (Fin 25)) -> (k : Vect c (Fin 25)) -> (q++l)++k = q++(l++k)
hak [] l k = Refl
hak {a=S a} {b=b} {c=c} (x::xs) l k = ?hole --booo x (rewrite plusAssociative a b c in ((xs++l)++k)) (xs++(l++k))  --(hak xs l k)
-------------------------------------------------------------------------------
uniqueConcat4 :
 (l : Vect n (Fin 25)) ->
 (k : Vect m (Fin 25)) ->
 (q : Vect o (Fin 25)) ->
 UniqueVect ((q ++ l) ++ k) ->
 UniqueVect l

uniqueConcat4 {n=n} {m=m} {o=o} l k q prf =
 let qlUnique = uniqueConcat2 (q++l) k prf in
 uniqueConcat n o l q qlUnique
-------------------------------------------------------------------------------
-- 1.17.1
-- And when Abram was ninety years old and nine, the LORD appeared to Abram,
-- and said unto him, I am the Almighty God; walk before me, and be thou perfect.
uniqueConcat3 : (l : Vect n (Fin 25)) -> (k : Vect m (Fin 25)) -> (q : Vect o (Fin 25)) -> UniqueVect (q ++ (l ++ k)) -> UniqueVect l
uniqueConcat3 {n=n} {m=m} {o=o} l k q prf = ?hole -- uniqueConcat4 l k q (rewrite hak q l k in prf)
-------------------------------------------------------------------------------
uniqueRemoveHead :
 (l : Vect (S n) (Fin 25)) ->
 UniqueVect l ->
 UniqueVect (tail l)

uniqueRemoveHead _ (UniqueConcat _ _ _ uniqueListT) = uniqueListT
-------------------------------------------------------------------------------
deleteAtHeadRemovesHead :
 (l : Vect (S n) (Fin 25)) ->
 deleteAt FZ l = tail l

deleteAtHeadRemovesHead (x::xs) = Refl
-------------------------------------------------------------------------------
deleteTailEquality :
 (y : Fin 25) ->
 (xs : Vect (S n) (Fin 25)) ->
 (fk : Fin (S n)) ->
 x :: (deleteAt fk xs) = deleteAt (FS fk) (x :: xs)

deleteTailEquality y (x::xs) fk = Refl
-------------------------------------------------------------------------------
