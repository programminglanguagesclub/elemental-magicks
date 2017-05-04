

module Lib.RedBlackTree


data Color : Type where
 Red : Color
 Black : Color


data RedBlackTree : (keyType : Type) -> (Ord keyType) => Type -> Color -> Nat -> Type where
 Empty : Ord keyType => RedBlackTree keyType valueType Black Z
 TreeRed : Ord keyType => RedBlackTree keyType valueType Black n -> (key : keyType) -> (value : valueType) -> RedBlackTree keyType valueType Black n -> RedBlackTree keyType valueType Red n
 TreeBlack : Ord keyType => RedBlackTree keyType valueType color1 n -> (key : keyType) -> (value : valueType) -> RedBlackTree keyType valueType color2 n -> RedBlackTree keyType valueType Black (S n)
