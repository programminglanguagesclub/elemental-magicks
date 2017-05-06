

module Lib.RedBlackTree


data Color : Type where
 Red : Color
 Black : Color


{-The root does not have to be black. In other words, this is a relaxed red black tree.-}
{-Also, the blackheight is the number of black nodes encountered minus 1-}
data RedBlackTree : (keyType : Type) -> (Ord keyType) => Type -> Color -> Nat -> Type where
 EmptyBlack : Ord keyType => RedBlackTree keyType valueType Black 0
 TreeRed : Ord keyType => RedBlackTree keyType valueType Black n -> (key : keyType) -> (value : valueType) -> RedBlackTree keyType valueType Black n -> RedBlackTree keyType valueType Red n
 TreeBlack : Ord keyType => RedBlackTree keyType valueType color1 n -> (key : keyType) -> (value : valueType) -> RedBlackTree keyType valueType color2 n -> RedBlackTree keyType valueType Black (S n)


{-
insert : RedBlackTree keyType valueType color blackHeight -> keyType -> valueType -> RedBlackTree`
-}


