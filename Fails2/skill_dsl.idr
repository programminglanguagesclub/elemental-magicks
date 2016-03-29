module Skill_dsl

import Data.Vect
import Data.Fin
import Data.So
import preliminaries
import objects


public export Env : Nat -> Nat -> Nat -> Type
Env m n p = (Vect m Monster, Vect n BoardIndex, Vect p Card)


public export data Area = SpawnPosition | HandArea | GraveyardArea | DiscardArea
public export data Side = Friendly | Enemy

public export Set : Type
Set = (Side, Area)


public export data CardExistential = DeBruijnCardExistential Set


public export data CardVar : Nat -> Type where
 BoundCardVar : Card -> CardVar 0
 UnBoundCardVar : (Fin n) -> (CardVar n)



public export data BoardMonsterExistential = DeBruijnBoardMonsterExistential Side
public export data BoardMonsterVar = BoundBoardMonsterVar Monster | UnBoundBoardMonsterVar (Fin n) {- so essentially what these mean is "the n-1th variable that is not yet bound... or something like that" -}


public export BoardMonsterPredicate : Type
BoardMonsterPredicate = Monster -> Bool


public export data BoardSquareExistential = DeBruijnBoardSquareExistential Side
public export data BoardSquareVar = BoundBoardSquareVar BoardIndex | UnBoundBoardSquareVar (Fin n)

public export BoardSquarePredicate : Type
BoardSquarePredicate = BoardIndex -> Bool




public export MonsterAlive : BoardMonsterPredicate
MonsterAlive m with (aliveness m)
 | Alive = True
 | _ = False





public export CardPredicate : Type
CardPredicate = Card -> Bool


public export HandIndex : Type
HandIndex = Bounded 0 29

public export GraveyardIndex : Type
GraveyardIndex = Bounded 0 29

public export data StatLValue = TemporaryL | PermanentL
public export data Mutator = IncrementL | SetL






public export BoardSquareCondition : Nat -> Type
BoardSquareCondition n = (Vect n BoardSquareExistential, BoardSquarePredicate)

public export BoardMonsterCondition : Nat -> Type
BoardMonsterCondition n = (Vect n BoardMonsterExistential, BoardMonsterPredicate)

public export CardCondition : Nat -> Type
CardCondition n = (Vect n CardExistential, CardPredicate)


public export data Condition : Nat -> Nat -> Nat -> Type where
 BoardMonsterCondition_ : (BoardMonsterCondition n) -> Condition n 0 0
 BoardSquareCondition_ : (BoardSquareCondition n) -> Condition n 0 0
 CardCondition_ : (CardCondition n) -> Condition 0 n 0

mutual
 public export NonautomaticSkillComponent : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type
 NonautomaticSkillComponent m n p env = {m' : Nat} -> {n' : Nat} -> {p' : Nat} -> ((Condition m' n' p'), AutomaticSkillComponent m n p env, AutomaticSkillComponent m n p env, AutomaticSkillComponent m n p env)
 public export AutomaticSkillComponent : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type
 AutomaticSkillComponent m n p env = (Maybe (NonautomaticSkillComponent m n p env))



public export Skill : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type
Skill m n p env = AutomaticSkillComponent m n p env



