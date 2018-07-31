

mplus : Nat -> Nat -> Nat
mplus x y =
 case x of
  Z => y
  S x' => S (mplus x' y)


mtimes : Nat -> Nat -> Nat
mtimes x y =
  case x of
   Z => Z
   S x' => mplus (mtimes x' y) y




