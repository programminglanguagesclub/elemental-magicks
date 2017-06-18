
module RedBlackTree.Extrinsic

%default total

data Color : Type where
 Red : Color
 Black : Color

data Tree : (keyType : Type) -> (valueType : Type) -> Type where
 Empty : Tree keyType valueType
 Node : Color -> (key : keyType) -> (value : valueType) -> Tree keyType valueType -> Tree keyType valueType -> Tree keyType valueType

balance : Color -> keyType -> valueType -> Tree keyType valueType -> Tree keyType valueType -> Tree keyType valueType
balance Black zk zv (Node Red yk yv (Node Red xk xv a b) c) d = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balance Black zk zv (Node Red xk xv a (Node Red yk yv b c)) d = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balance Black xk xv a (Node Red zk zv (Node Red yk yv b c) d) = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balance Black xk xv a (Node Red yk yv b (Node Red zk zv c d)) = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balance c k v l r = Node c k v l r


ins : keyType -> Ord keyType => valueType -> Tree keyType valueType -> Tree keyType valueType
ins k v Empty = Node Red k v Empty Empty
ins k v (Node c k' v' l r) with (compare k k')
                                | LT = balance c k' v' (ins k v l) r
                                | EQ = Node c k v l r
                                | GT = balance c k' v' l (ins k v r)


blackenRoot : Tree keyType valueType -> Tree keyType valueType
blackenRoot Empty = Empty
blackenRoot (Node _ k v l r) = Node Black k v l r

insert : keyType -> Ord keyType => valueType -> Tree keyType valueType -> Tree keyType valueType
insert k v t = blackenRoot (ins k v t)

-------------------------------------------------------------------------------

data ColorHeight : Color -> Nat -> Nat -> Type where
 CH_Red : ColorHeight Red n n
 CH_Black : ColorHeight Black n (S n)

data HasBH : Tree keyType valueType -> Nat -> Type where
 HBH_Empty : HasBH Empty 1
 HBH_Node :
  HasBH l n ->
  ColorHeight c n m ->
  HasBH r n ->
  ------------
  HasBH (Node c k v l r) m

{-
data HasBH : Tree keyType valueType -> Nat -> Type where
 HBH_Empty : HasBH Empty 1
 HBH_Node_Red :
  (n : Nat) ->
  (k : keyType) ->
  (v : valueType) ->
  (l : Tree keyType valueType) ->
  (r : Tree keyType valueType) ->
  HasBH l n ->
  HasBH r n ->
  ------------
  HasBH (Node Red k v l r) n

 HBH_Node_Black :
  (n : Nat) ->
  (k : keyType) ->
  (v : valueType) ->
  (l : Tree keyType valueType) ->
  (r : Tree keyType valueType) ->
  HasBH l n ->
  HasBH r n ->
  ------------
  HasBH (Node Black k v l r) (S n)
-}

----------------------------------------------------
blackenRoot_bh : HasBH t n -> (m ** (HasBH (blackenRoot t) m))
blackenRoot_bh HBH_Empty = (1 ** HBH_Empty)
blackenRoot_bh (HBH_Node hl hc hr) = (_ ** (HBH_Node hl CH_Black hr))

{-
blackenRoot_bh : (t : Tree keyType valueType) -> (n : Nat) -> HasBH t n -> (m ** (HasBH (blackenRoot t) m))
blackenRoot_bh Empty n h = (1 ** HBH_Empty)
blackenRoot_bh (Node Red k v l r) n (HBH_Node_Red n k v l r hl hr) = ((S n) ** (HBH_Node_Black n k v l r hl hr))
blackenRoot_bh (Node Black k v l r) (S n) (HBH_Node_Black n k v l r hl hr) = ((S n) ** (HBH_Node_Black n k v l r hl hr))

ins_bh : {k : keyType} -> Ord keyType => {v : valueType} -> {t : Tree keyType valueType} -> {n : Nat} -> HasBH t n -> HasBH (ins k v t) n
ins_bh HBH_Empty = ?hole

-}



{-
blackenRoot_bh (Node Black k v l r) n h = (n **(    ))
-}


------------------------------------------------------


balance_bh :
 (key : keyType) ->
 (value : valueType) ->
 (l : Tree keyType valueType) ->
 (r : Tree keyType valueType) ->
 ColorHeight c n m ->
 HasBH l n ->
 HasBH r n ->
 HasBH (balance c key value l r) m

balance_bh key value l r hc hl hr = ?hole



-------------------------------------------------------
ins_bh : (key : keyType) -> Ord keyType => (value : valueType) -> (t : Tree keyType valueType) -> HasBH t n -> HasBH (ins key value t) n
ins_bh key value Empty h {n=S Z} = HBH_Node HBH_Empty CH_Red HBH_Empty
ins_bh key value (Node color k v l r) (HBH_Node hl hc hr) {n=n} with (compare key k)
{-
 {-| (LT, Node Black zk zv (Node Red yk yv (Node Red xk xv a b) c) d) = ?hole
 | (LT, Node Black zk zv (Node Red xk xv a (Node Red yk yv b c)) d) = ?hole
 | (LT, Node Black xk xv a (Node Red zk zv (Node Red yk yv b c) d)) = ?hole
 | (LT, Node Black xk xv a (Node Red yk yv b (Node Red zk zv c d))) = ?hole-}
 | (LT, Node c' k' v' l' r') = believe_me (HBH_Node (ins_bh key value l hl) _ hr) -- WRONG


{-
  let ins_l_bh = the (HasBH (ins key value l) _) (ins_bh key value l hl) in
  let l' = ins key value l in
  case l' of
   (Node Red yk yv (Node Red xk xv a b) c) => ?hole
   (Node Red xk xv a (Node Red yk yv b c)) => ?hole
   a =>
    case r of
     (Node Red zk zv (Node Red yk yv b c) d) => ?hole
     (Node Red yk yv b (Node Red zk zv c d)) => ?hole
     _ => HBH_Node (the (HasBH (ins key value l) _) ins_l_bh) hc hr
-}-} 

 | LT = balance_bh k v (ins key value l) r hc (ins_bh key value l hl) hr
 | EQ = HBH_Node hl hc hr
 | GT = balance_bh k v l (ins key value r) hc hl (ins_bh key value r hr)


{-

 HBH_Node :
  HasBH l n ->
  ColorHeight c n m ->
  HasBH r n ->
  ------------
  HasBH (Node c k v l r) m


-}



{-

ins : keyType -> Ord keyType => valueType -> Tree keyType valueType -> Tree keyType valueType
ins k v Empty = Node Red k v Empty Empty
ins k v (Node c k' v' l r) with (compare k k')
                                | LT = balance c k' v' (ins k v l) r
                                | EQ = Node c k v l r
                                | GT = balance c k' v' l (ins k v r)



balance : Color -> keyType -> valueType -> Tree keyType valueType -> Tree keyType valueType -> Tree keyType valueType
balance Black zk zv (Node Red yk yv (Node Red xk xv a b) c) d = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balance Black zk zv (Node Red xk xv a (Node Red yk yv b c)) d = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balance Black xk xv a (Node Red zk zv (Node Red yk yv b c) d) = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balance Black xk xv a (Node Red yk yv b (Node Red zk zv c d)) = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balance c k v l r = Node c k v l r



-}











-----------------------------------------------





{-

balance_bh :
 {c : Color} ->
 {k : keyType} ->
 {v : valueType} ->
 {l : Tree keyType valueType} ->
 {r : Tree keyType valueType} ->
 {n : Nat} ->
 {m : Nat} ->
 HasBH l n ->
 ColorHeight c n m ->
 HasBH r n ->
 HasBH (balance c k v l r) m



{-

balance : Color -> keyType -> valueType -> Tree keyType valueType -> Tree keyType valueType -> Tree keyType valueType
balance Black zk zv (Node Red yk yv (Node Red xk xv a b) c) d = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balance Black zk zv (Node Red xk xv a (Node Red yk yv b c)) d = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balance Black xk xv a (Node Red zk zv (Node Red yk yv b c) d) = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balance Black xk xv a (Node Red yk yv b (Node Red zk zv c d)) = Node Red yk yv (Node Black xk xv a b) (Node Black zk zv c d)
balance c k v l r = Node c k v l r


-}

{-
balance_bh {c} {k} {v} {(Node Red yk yv (Node Red xk xv a b) c)} {r} {n} {m} hbhl ch hbhr = ?hole
-}

balance_bh {c} {k} {v} {l} {r} {n} {m} hbhl ch hbhr = ?hole

{-
balance_bh {c} {k} {v} {l} {r} {n} {m} hbhl ch hbhr =
balance_bh {c} {k} {v} {l} {r} {n} {m} hbhl ch hbhr =
-}

-}



testTree : Tree Int Int
testTree = Empty

foo : HasBH Empty 1
foo = HBH_Empty








