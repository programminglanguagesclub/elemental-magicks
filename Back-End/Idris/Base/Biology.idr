module Base.Biology
import Data.So
import Data.Vect
import Base.Utility
import Base.Bounded
import Base.Preliminaries
%access public export
%default total

-- This module is to contain the various datatypes
-- that are used by the Science and Religion modules.
-------------------------------------------------------------------------------
data UniqueVect : {n : Nat} -> Vect n (Fin 25) -> Type where
 UniqueEmpty : UniqueVect []
 UniqueConcat :
  (xs : Vect n (Fin 25)) ->
  (x : Fin 25) ->
  Not (Elem x xs) ->
  UniqueVect xs ->
  UniqueVect (x :: xs)

data IndexedUniqueVect : (n : Nat) -> Vect n (Fin 25) -> Type where
 IndexedUniqueEmpty : IndexedUniqueVect Z []
 IndexedUniqueConcat :
  (n : Nat) ->
  (xs : Vect n (Fin 25)) ->
  (x : Fin 25) ->
  Not (Elem x xs) ->
  IndexedUniqueVect n xs ->
  IndexedUniqueVect (S n) (x :: xs)

toIndexed : (n : Nat) -> (v : Vect n (Fin 25)) -> UniqueVect v -> IndexedUniqueVect n v
toIndexed Z [] UniqueEmpty = IndexedUniqueEmpty
toIndexed (S k) (x::xs) (UniqueConcat xs x notXXS uniqueXS) = IndexedUniqueConcat k xs x notXXS (toIndexed k xs uniqueXS)

fromIndexed : (n : Nat) -> (v : Vect n (Fin 25)) -> IndexedUniqueVect n v -> UniqueVect v
fromIndexed Z [] IndexedUniqueEmpty = UniqueEmpty
fromIndexed (S k) (x::xs) (IndexedUniqueConcat k xs x notXXS indexedUniqueXS) = UniqueConcat xs x notXXS (fromIndexed k xs indexedUniqueXS)
