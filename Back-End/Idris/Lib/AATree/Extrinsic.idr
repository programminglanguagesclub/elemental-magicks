module AA.Extrinsic

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



-------------------------------------------------------------------------------
level :
 Tree keyType valueType ->
 Nat

level Leaf = 0
level (Branch n _ _ _ _) = n
-------------------------------------------------------------------------------
setLevel :
 Nat ->
 Tree keyType valueType ->
 Tree keyType valueType

setLevel _ Leaf = Leaf
setLevel n (Branch m k v l r) = Branch n k v l r
-------------------------------------------------------------------------------
skew :
 Tree keyType valueType ->
 Tree keyType valueType

skew Leaf = Leaf
skew (Branch n k v Leaf r) = Branch n k v Leaf r
skew (Branch n k1 v1 (Branch m k2 v2 ll lr) r) =
 case decEq n m of
  Yes _ => Branch m k2 v2 ll (Branch n k1 v1 lr r)
  No _ => Branch n k1 v1 (Branch m k2 v2 ll lr) r
-------------------------------------------------------------------------------
split :
 Tree keyType valueType ->
 Tree keyType valueType

split Leaf = Leaf
split (Branch n k v l Leaf) = Branch n k v l Leaf
split (Branch n k1 v1 l (Branch m k2 v2 rl Leaf)) = Branch n k1 v1 l (Branch m k2 v2 rl Leaf)
split (Branch n k1 v1 l (Branch m k2 v2 rl rr)) =
 case decEq n (level rr) of
  Yes _ => Branch (S m) k2 v2 (Branch n k1 v1 l rl) rr
  No _ => Branch n k1 v1 l (Branch m k2 v2 rl rr)
-------------------------------------------------------------------------------
insert :
 (key : keyType) ->
 (value : valueType) ->
 Ord keyType =>
 Tree keyType valueType ->
 Tree keyType valueType

-- inserting a key equal to an existing key will overwrite its key and value.

insert key value Leaf = Branch 1 key value Leaf Leaf
insert key value (Branch n k v l r) with (compare key k)
 | LT = split $ skew $ Branch n k v (insert key value l) r
 | EQ = Branch n key value l r
 | GT = split $ skew $ Branch n k v l (insert key value r)
-------------------------------------------------------------------------------
decreaseLevel :
 Tree keyType valueType ->
 Tree keyType valueType

decreaseLevel Leaf = Leaf
decreaseLevel (Branch n k v l r) =
 let m = S (min (level l) (level r)) in
 Branch m k v l (setLevel (min m (level r)) r)
-------------------------------------------------------------------------------
{-
rebalance :
 Tree keyType valueType ->
 Tree keyType valueType

rebalance Leaf = Leaf
rebalance (Branch n k v l r) = skew $ decreaseLevel (Branch n k v l r) WRONG
-}
-------------------------------------------------------------------------------
{-
delete :
 (key : keyType) ->
 Ord keyType =>
 Tree keyType valueType ->
 Tree keyType valueType

delete _ Leaf = Leaf
delete key (Branch n k v l r) with (compare key k)
 | LT = ?hole {- also have rebalance and stuff delete key l-}
 | EQ = ?hole
 | GT = ?hole {- also have rebalanace adns tuff delete key r-}
-}
-------------------------------------------------------------------------------
show :
 Tree keyType valueType ->
 Show keyType =>
 Show valueType =>
 String

{-show Leaf = {-"🍂"-}-}

show Leaf = "L"
show (Branch n k v l r) =
 "(" ++
 (show n) ++
 "," ++
 (show k) ++
 ":" ++
 (show v) ++
 (show l) ++
 (show r) ++
 ")"



-------------------------------------------------------------------------------

-- Extrinsic verification

{-

From Wikipedia (https://en.wikipedia.org/wiki/AA_tree)

1. The level of every leaf node is one.
2. The level of every left child is exactly one less than that of its parent.
3. The level of every right child is equal to or one less than that of its parent.
4. The level of every right grandchild is strictly less than that of its grandparent.
5. Every node of level greater than one has two children.

Note that rule 5 is already intrinsic.
-}

data Level_Correct : Tree keyType valueType -> Nat -> Type where

--Inference Rule 1. 
 Level_Correct_Leaf :
 
  --------------------
  Level_Correct Leaf 0

--Inference Rule 2. Assuming no right grandchild.
 Level_Correct_Branch_No_Right_Grandchild :

 (p : Nat) ->
 (k : keyType) ->
 (v : valueType) ->
 (l : Tree keyType valueType) ->
 Level_Correct l n ->
 p = (S n) -> -- Wikipedia Rule 2.
 Either (p = 0) (p = 1) -> -- Wikipedia Rule 3.
 --------------------------------
 Level_Correct (Branch p k v l Leaf) p
 
--Inference Rule 2. Assuming the presence of a right grandchild.
 Level_Correct_Branch_With_Right_Grandchild :

 (p1 : Nat) ->
 (k1 : keyType) ->
 (v1 : valueType) ->
 (l : Tree keyType valueType) ->
 (p2 : Nat) ->
 (k2 : keyType) ->
 (v2 : valueType) ->
 (rl : Tree keyType valueType) ->
 (rr : Tree keyType valueType) ->
 Level_Correct l n1 ->
 Level_Correct (Branch p2 k2 v2 rl rr) p2 ->
 Level_Correct rl n2 ->
 Level_Correct rr n3 ->
 p = S n1 -> -- Wikipedia Rule 2.
 Either (p = p2) (p = S p2) -> -- Wikipedia Rule 3.
 LT n3 p1 -> -- Wikipedia Rule 4.
 ------------------------------------------------------------
 Level_Correct (Branch p1 k1 v1 l (Branch p2 k2 v2 rl rr)) p1
 
-------------------------------------------------------------------------------


insert_lc : (key : keyType) -> Ord keyType => (value : valueType) -> (t : Tree keyType valueType) -> Level_Correct t n -> (m ** (Level_Correct (insert key value t) m))
insert_lc key value Leaf Level_Corect_Leaf = (1 ** Level_Correct_Branch_No_Right_Grandchild 1 key value Leaf (the (Level_Correct Leaf 0) Level_Correct_Leaf) Refl (Right Refl))

insert_lc key value (Branch n k v l r) prf = ?hole



{-insert key value Leaf = Branch 1 key value Leaf Leaf
-}




