module Skill_dsl

import Data.Vect
import Data.Fin
import Data.So
import preliminaries
import objects


public export Env : Nat -> Nat -> Nat -> Type
Env m n p = (Vect m Monster, Vect n BoardIndex, Vect p Card)

public export empty_env : Env 0 0 0
empty_env = ([],[],[])

{- For now, I am not putting the requirement that entries be unique into the type. This is exportANT, but it is unclear to me now where it should be handled (could even be done in Ur/Web) -}
{-also for now cannot target discard.-}

public export data Area = SpawnPosition | HandArea | GraveyardArea | DiscardArea
public export data Side = Friendly | Enemy

{- For now ignoring the client update part of this -}
{- I might be able to get away with doing a lot more of this with type classes -}


public export Set : Type
Set = (Side, Area)

{- not implementing universal quantifiers yet -}

public export data CardExistential = DeBruijnCardExistential Set

{-not sure if I want Fin n or Nat here...-}

public export data CardVar : Nat -> Type where
 BoundCardVar : Card -> CardVar 0
 UnBoundCardVar : (Fin n) -> (CardVar n)


{- The trick here is that for the purpose of selecting squares on the board I don't really want "Maybe Monster". If they select a monster,
I want that to be the same monster even if it gets moved on the board. In the case of an empty square, however, it has to be that particular coordinate.

It's worth nothing though that this means that in the case of empty squares that are no longer empty, we have a problem.

Equally though, we have an issue with monsters that have moved to a region that does not allow certain skills to be applied with them in their former capacity.


For now, it's acceptable to represent the square number and monster at the type level, and have a value level check to see if it's still valid.

Then if it's not valid, the skill can terminate (or at least that part of it).

This may be the best solution in the end because we may wish to allow skills to have the possibility of doing this depending on
game state or player action.

-}

{- Might need another type for the Spawn position?? -}
{- Rename set to spawn everywhere -}

{- the idea is that once everything is bound, n will be 0, and we can just extract the card via matching -}

public export data BoardMonsterExistential = DeBruijnBoardMonsterExistential Side
public export data BoardMonsterVar = BoundBoardMonsterVar Monster | UnBoundBoardMonsterVar (Fin n) {- so essentially what these mean is "the n-1th variable that is not yet bound... or something like that" -}

public export BoardMonsterPredicate : Type
BoardMonsterPredicate = Monster -> Bool

public export data BoardSquareExistential = DeBruijnBoardSquareExistential Side
public export data BoardSquareVar = BoundBoardSquareVar BoardIndex | UnBoundBoardSquareVar (Fin n)

{-I might need more information... I Might need to pass in the game object into these really...
-}
public export BoardSquarePredicate : Type
BoardSquarePredicate = BoardIndex -> Bool

public export MonsterAlive : BoardMonsterPredicate
MonsterAlive m with (aliveness m)
 | Alive = True
 | _ = False

public export CardPredicate : Type
CardPredicate = Card -> Bool

public export data StatLValue = TemporaryL | PermanentL
public export data Mutator = IncrementL | SetL

public export data StatRValue = TemporaryR | PermanentR | BaseR


{-currently no way to get attack from monster in hand, etc (as it might not be a monster). also ignoring set position for now -}
public export data LazyInt : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type where
 {-Constant : Integer -> LazyInt 0 -} {-ignoring constant for now. It's neither bound to a card nor awaiting binding, so it's an edge case for this design-}
 BoardAttackR : {m : Nat} -> {n : Nat} -> {p : Nat} -> {env : Env m n p} -> StatRValue -> Fin n -> LazyInt m n p env
 BoardDefenseR : {m : Nat} -> {n : Nat} -> {p : Nat} -> {env : Env m n p} -> StatRValue -> Fin n -> LazyInt m n p env
 BoardRangeR : {m : Nat} -> {n : Nat} -> {p : Nat} -> {env : Env m n p} -> StatRValue -> Fin n -> LazyInt m n p env
 BoardSpeedR : {m : Nat} -> {n : Nat} -> {p : Nat} -> {env : Env m n p} -> StatRValue -> Fin n -> LazyInt m n p env
             
{-
             | SchoolR School
             | ThoughtsR
             | SoulPointsR
             {- ignoring a few nice things like cardinality for now -}
-}

{- Need to keep track of the accessing index so Fin n Fin m Fin p goes somewhere around here... in LazyInt somewhere? How do I fit it? -}
{- can I write {n m p : Nat} -> ??? -}
public export data SkillEffect : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type where
 AttackL : {m : Nat} -> {n : Nat} -> {p : Nat} -> {env : Env m n p} -> Mutator -> StatLValue -> Side -> BoardIndex -> (LazyInt m n p env) -> SkillEffect m n p env
 DefenseL : {m : Nat} -> {n : Nat} -> {p : Nat} -> {env : Env m n p} -> Mutator -> StatLValue -> Side -> BoardIndex -> (LazyInt m n p env) -> SkillEffect m n p env
 RangeL : {m : Nat} -> {n : Nat} -> {p : Nat} -> {env : Env m n p} -> Mutator -> StatLValue -> Side -> BoardIndex -> (LazyInt m n p env) -> SkillEffect m n p env
 LevelL : {m : Nat} -> {n : Nat} -> {p : Nat} -> {env : Env m n p} -> Mutator -> StatLValue -> Side -> BoardIndex -> (LazyInt m n p env) -> SkillEffect m n p env
 SpeedL : {m : Nat} -> {n : Nat} -> {p : Nat} -> {env : Env m n p} -> Mutator -> StatLValue -> Side -> BoardIndex -> (LazyInt m n p env) -> SkillEffect m n p env

{-
 Refresh Side BoardIndex {-if alive, restores all stats to base... could maybe not have this be a core thing. could even create syntactic sugar for this with syntax.... -}
 |IncrementHp BoardIndex LazyInt {- to be revisted when hp is defined -}
 |IncrementMaxHp BoardIndex LazyInt {- to be revisted when hp is defined -}
 |SendFromFieldToHand Side BoardIndex
 |SendFromFieldToGraveyard Side BoardIndex GraveyardIndex {-etc, but we can also abstract over FROM and TO...-}
 |SendFromGraveyardToGraveyard Side GraveyardIndex GraveyardIndex
 |SendFromGraveyardToHand Side GraveyardIndex HandIndex
 |SendFromHandToGraveyard Side HandIndex GraveyardIndex
 |SendFromHandToHand Side HandIndex HandIndex
 |SwitchSetAndHand Side HandIndex
 |SwitchFieldAndHand Side BoardIndex HandIndex
 |SwitchFieldAndGraveyard Side BoardIndex GraveyardIndex
 |SwitchFieldAndSet Side BoardIndex
 |SwitchHandAndSet Side HandIndex
 |SwitchGraveyardAndSet Side GraveyardIndex
 |SendFromHandToSet Side HandIndex{- no effect if set occupied or no monsters in hand -}
 |SendFromHandToField Side HandIndex BoardIndex {- no effect if field occupied at determined index or no monsters in hand -} 
 |SendFromGraveyardToField Side GraveyardIndex BoardIndex {- no effect if field occupied at determined index or no monsters in graveyard -}
{- there might be some more cases I'm forgetting right now.. -}
 |SetHp Side BoardIndex LazyInt {- to be revisted when hp is defined -}
 |SetMaxHp Side BoardIndex LazyInt {- to be revisted when hp is defined -}
 |ReviveCard Side BoardIndex
 |EngagementL Side BoardIndex LazyInt
 |ThoughtsL Side Mutator LazyInt
 |KnowledgeL Mutator School Side LazyInt
 |TakeDamage Side BoardIndex LazyInt

-}


{-Counter Skills should not trigger more than once per <SkillQueue is cleared completely> This way I don't have to worry about players running out of time from massively titanic runs of counterskill chains, or even nontermination-}



{- in order to allow for universals and existentials, I should not require BoardIndex here to be predetermined.... -}


{-
public export FooBlargFoo : SkillEffect
FooBlargFoo = SpeedL SetL TemporaryL Friendly (2 ** Oh) (Constant 4)
-}



public export BoardSquareCondition : Nat -> Type
BoardSquareCondition n = (Vect n BoardSquareExistential, BoardSquarePredicate)

public export BoardMonsterCondition : Nat -> Type
BoardMonsterCondition n = (Vect n BoardMonsterExistential, BoardMonsterPredicate)

public export CardCondition : Nat -> Type
CardCondition n = (Vect n CardExistential, CardPredicate)

{- Again, actually need 2-3 Nats. -}


{-Nats refer to: Board, and then Graveyard | Hand | Discard, and then Spawn position.-}



{- Need a way to combine this with earlier conditions so that the predicate can reference earlier existentials... -}

{- for now ignoring whatever a set condition would be. It's not an existential. Maybe a universal...-}

public export data Condition : {-Nat -> Nat -> Nat ->-} Nat -> Nat -> Nat -> Type where
 BoardMonsterCondition_ : (BoardMonsterCondition n) -> Condition n 0 0
 BoardSquareCondition_ : (BoardSquareCondition n) -> Condition n 0 0
 CardCondition_ : (CardCondition n) -> Condition 0 n 0




