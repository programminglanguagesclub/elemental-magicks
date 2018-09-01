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
data UniqueVect : (n : Nat) -> Vect n (Fin 25) -> Type where
 UniqueEmpty : UniqueVect Z []
 UniqueConcat :
  (n : Nat) ->
  (xs : Vect n (Fin 25)) ->
  (x : Fin 25) ->
  Not (Elem x xs) ->
  UniqueVect n xs ->
  UniqueVect (S n) (x :: xs)


