

plus : Nat -> Nat -> Nat
plus x y =
 case x of
  Z => y
  S x' => S (plus x' y)


times : Nat -> Nat -> Nat
times x y =
  case x of
   Z => Z
   S x' => plus (times x' y) y




