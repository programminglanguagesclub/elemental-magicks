

data TreeShape
 = Leaf
 | Branch TreeShape TreeShape

foo : (t : TreeShape) -> Dec (Leaf = t)
foo Leaf = Yes Refl
foo (Branch _ _ ) = No ?hole




leafNotBranch : (Branch (_t) (_t1) = Leaf) -> Void
leafNotBranch Refl impossible

m : (x : TreeShape) -> Dec (x = Leaf)
m Leaf = Yes Refl
m (Branch _ _ ) = No leafNotBranch

leafNotBranch1 : (Leaf = Branch (Branch a b) c) -> Void
leafNotBranch1 Refl impossible


correctBranchCase1 : Branch (Branch a b) c = Branch (Branch a1 b1) c1

n : (x : TreeShape) -> Dec (x = (Branch (Branch a b) c))
n Leaf = No leafNotBranch1
n (Branch (Branch a b) c) = Yes correctBranchCase1




bar : (x : TreeShape) -> TreeShape
bar Leaf = Leaf
bar (Branch (Branch a b) c) = (Branch (Branch a b) c)
bar (Branch a (Branch b c)) = (Branch a (Branch b c))
bar a = a


baz : (x : TreeShape) -> bar x = x
baz Leaf = Refl
baz (Branch (Branch a b) c) = Refl
--baz (Branch a (Branch b c)) = Refl



{-
foo : (x : Integer) -> DecEq (1 = x)
foo x with (compare 1 x)
 | EQ = ?hole
 | LT = ?hole
 | GT = ?hole



bar : Integer -> Integer
bar 1 = 2
bar 1 = 4
bar x = 2*x


baz : (x : Integer) -> bar x = 2*x
baz 1 = Refl
baz 2 = Refl
baz x = ?hole
-}










{-






















data TreeShape
 = Leaf
 | Branch TreeShape TreeShape

foo : (t : TreeShape) -> Dec (Leaf = t)
foo Leaf = Yes Refl
foo (Branch _ _ ) = No ?hole


bar : TreeShape -> TreeShape
bar Leaf = Leaf
bar (Branch (Branch a b) c) = (Branch (Branch a b) c)
bar (Branch a (Branch b c)) = (Branch a (Branch b c))
bar a = a


baz : (x : TreeShape) -> bar x = x
baz Leaf = Refl
baz (Branch (Branch a b) c) = Refl
baz (Branch a (Branch b c)) = Refl












-}
