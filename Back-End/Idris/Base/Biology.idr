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
data UniqueVect : Vect n (Fin 25) -> Type where
 UniqueEmpty : UniqueVect []
 UniqueConcat :
  (xs : Vect n (Fin 25)) ->
  (x : Fin 25) ->
  Not (Elem x xs) ->
  UniqueVect xs ->
  UniqueVect (x :: xs)


