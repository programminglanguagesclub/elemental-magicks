module ServerUpdates

import Data.Vect
import Data.Fin
import Data.So
import preliminaries
import phase
import objects_basic
import objects_advanced
import skill_dsl


public export
data ServerUpdate : Type where
 SetCard : Schools -> {-(Bounded 0 25)-} Nat -> ServerUpdate
 Skip : Schools -> ServerUpdate
 AttackRow : (Fin 3) -> ServerUpdate
 Rest : ServerUpdate
 DirectAttack : ServerUpdate
 Move : BoardIndex -> ServerUpdate
 SkillInitiation : Nat -> ServerUpdate
 SkillSelection : (List Nat, List Nat, List Nat, List Nat, List Nat, List Nat) -> ServerUpdate
 Revive : Vect 9 Bool -> ServerUpdate
 DrawCard : Nat -> ServerUpdate {-The natural number is the ID of the card in some representation. For now this should be stored in Idris, though Ur/Web could also participate eventually by storing a database.-}

public export
ServerUpdateWrapper : Type
ServerUpdateWrapper = (String, ServerUpdate)
