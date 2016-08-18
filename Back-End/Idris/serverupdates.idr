module ServerUpdates

import Data.Vect
import Data.Fin
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import preliminaries
import phase
import objects_basic
import player
import game
import skill_dsl


public export
data ServerUpdate : Type where
 SpawnCard : Vect 6 (Bounded 0 9) -> {-(Bounded 0 25)-} Nat -> ServerUpdate
 Skip : Vect 6 (Bounded 0 9) -> ServerUpdate
 DeployCard : Fin 9 -> ServerUpdate
 AttackRow : (Fin 3) -> ServerUpdate
 Rest : ServerUpdate
 DirectAttack : ServerUpdate
 Move : Fin 9 -> ServerUpdate
 SkillInitiation : Nat -> ServerUpdate
 SkillSelection : (List Nat, List Nat, List Nat, List Nat, List Nat, List Nat) -> ServerUpdate
 Revive : Vect 9 Bool -> ServerUpdate
 DrawCard : Nat -> ServerUpdate {-The natural number is the ID of the card in some representation. For now this should be stored in Idris, though Ur/Web could also participate eventually by storing a database.-}

public export
ServerUpdateWrapper : Type
ServerUpdateWrapper = (String, ServerUpdate)
