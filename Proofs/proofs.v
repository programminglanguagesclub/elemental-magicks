(*This is done with Nat, rather than Integer,
but it shouldn't matter for these proofs.*)


Require Import Arith.

Theorem bar : forall (l : nat) (n : nat) (m : nat) , l <= n -> n <= m -> l <= m.
intros.
rewrite H.
apply H0.
Qed.


Theorem foobar : forall (n : nat), n <= n.
intros.
auto.
Qed.
