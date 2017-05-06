module AA.Extrinsic

%default total


data Tree : (keyType : Type) -> (valueType : Type) -> Type where
 Leaf : Tree keyType valueType
 Branch :
  Nat ->
  Tree keyType valueType ->
  Tree keyType valueType ->
  Tree keyType valueType


skew : Tree keyType valueType -> Tree keyType valueType
skew Leaf = Leaf
skew (Branch n Leaf r) = Branch n Leaf r
skew (Branch n (Branch m ll lr) r) =
 case decEq n m of
  Yes _ => Branch n ll (Branch n lr r)
  No _ => Branch n (Branch m ll lr) r

split : Tree keyType valueType -> Tree keyType valueType
split Leaf = Leaf
split (Branch n l Leaf) = Branch n l Leaf
split (Branch n l (Branch m rl Leaf)) = Branch n l (Branch m rl Leaf)
split (Branch n l (Branch m rl rr)) =
 case decEq n m of
  Yes _ => Branch (S m) (Branch n l rl) rr
  No _ => Branch n l (Branch m rl rr)




