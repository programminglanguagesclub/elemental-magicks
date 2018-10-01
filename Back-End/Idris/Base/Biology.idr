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
data UniqueVect : {n : Nat} -> Vect n a -> Type where
 UniqueEmpty : UniqueVect []
 UniqueConcat :
  (xs : Vect n a) ->
  (x : a) ->
  Not (Elem x xs) ->
  UniqueVect xs ->
  UniqueVect (x :: xs)

data IndexedUniqueVect : (n : Nat) -> Vect n a -> Type where
 IndexedUniqueEmpty : IndexedUniqueVect Z []
 IndexedUniqueConcat :
  (n : Nat) ->
  (xs : Vect n a) ->
  (x : a) ->
  Not (Elem x xs) ->
  IndexedUniqueVect n xs ->
  IndexedUniqueVect (S n) (x :: xs)

toIndexed : (n : Nat) -> (v : Vect n a) -> UniqueVect v -> IndexedUniqueVect n v
toIndexed Z [] UniqueEmpty = IndexedUniqueEmpty
toIndexed (S k) (x::xs) (UniqueConcat xs x notXXS uniqueXS) = IndexedUniqueConcat k xs x notXXS (toIndexed k xs uniqueXS)

fromIndexed : (n : Nat) -> (v : Vect n a) -> IndexedUniqueVect n v -> UniqueVect v
fromIndexed Z [] IndexedUniqueEmpty = UniqueEmpty
fromIndexed (S k) (x::xs) (IndexedUniqueConcat k xs x notXXS indexedUniqueXS) = UniqueConcat xs x notXXS (fromIndexed k xs indexedUniqueXS)
