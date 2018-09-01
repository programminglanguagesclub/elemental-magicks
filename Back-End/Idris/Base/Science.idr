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


