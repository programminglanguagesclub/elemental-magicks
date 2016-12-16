module Main.End_phase
import Base.Player
import Main.Game
import Base.Clientupdates
%access public export
%default total


stepEndPhase : WhichPlayer -> List Nat -> Player -> Player -> (Game, List ClientUpdate, Maybe ClientInstruction)
stepEndPhase initiative deathQueue player opponent = ?hole



{-


Have to iterate through the fields, potentially triggering unused end skills




findIndex : (elem -> Bool) -> Vect len elem -> Maybe (Fin len)
findIndex p []        = Nothing
findIndex p (x :: xs) = if p x then Just 0 else map FS (findIndex p xs)



{- written by the mighty Melvar -}
findIndexFrom : (a -> Bool) -> Fin n -> Vect n a -> Maybe (Fin n)
findIndexFrom p FZ xs = findIndex p xs
findIndexFrom p (FS k) (x :: xs) = FS <$> findIndexFrom p k xs

{- written by the mighty Melvar -}
findIndexPreferentiallyFrom : (a -> Bool) -> Fin n -> Vect n a -> Maybe (Fin n)
findIndexPreferentiallyFrom p FZ xs =  findIndex p xs
findIndexPreferentiallyFrom p (FS k) (x :: xs) = if p x then FS <$> findIndexFrom p k xs <|> Just FZ else FS <$> findIndexPreferentiallyFrom p k xs


-}



