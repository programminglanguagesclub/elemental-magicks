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


public export data BoardMonsterExistential = DeBruijnBoardMonsterExistential Side
public export data BoardMonsterVar = BoundBoardMonsterVar Monster | UnBoundBoardMonsterVar (Fin n) {- so essentially what these mean is "the n-1th variable that is not yet bound... or something like that" -}


public export BoardMonsterPredicate : Type
BoardMonsterPredicate = Monster -> Bool



public export BoardMonsterCondition : Nat -> Type
BoardMonsterCondition n = (Vect n BoardMonsterExistential, BoardMonsterPredicate)



public export data Condition : Nat -> Nat -> Nat -> Type where
 BoardMonsterCondition_ : (BoardMonsterCondition n) -> Condition n 0 0


mutual
 public export NonautomaticSkillComponent : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type

 {- TO PRODUCE A STACK OVERFLOW INSTEAD OF NON-TERMINATION, CHANGE THE FOLLOWING LINE TO:
  NonautomaticSkillComponent m n p env = {m' : Nat} -> {n' : Nat} -> {p' : Nat} -> (AutomaticSkillComponent m n p env, AutomaticSkillComponent m n p env, AutomaticSkillComponent m n p env)

 -}
 NonautomaticSkillComponent m n p env = {m' : Nat} -> {n' : Nat} -> {p' : Nat} -> ((Condition m' n' p'), AutomaticSkillComponent m n p env, AutomaticSkillComponent m n p env, AutomaticSkillComponent m n p env)
 

 public export AutomaticSkillComponent : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type
 AutomaticSkillComponent m n p env = (Maybe (NonautomaticSkillComponent m n p env))



public export Skill : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type
Skill m n p env = AutomaticSkillComponent m n p env



