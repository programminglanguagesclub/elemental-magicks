module AVL.Extrinsic

%default total



data Tree : (keyType : Type) -> (valueType : Type) -> Type where
 Leaf : Tree keyType valueType
 Branch :
  Nat ->
  keyType ->
  valueType ->
  Tree keyType valueType ->
  Tree keyType valueType ->
  Tree keyType valueType




