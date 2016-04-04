module ServerUpdates

import Data.Vect
import Data.Fin
import Data.So
import preliminaries
import phase
import objects
import skill_dsl


public export
data ServerUpdate : Type where
 SetCard : Schools -> (Bounded 0 25) -> ServerUpdate
 Skip : Schools -> ServerUpdate
 AttackRow : (Bounded 0 3) -> ServerUpdate
 Rest : ServerUpdate
 DirectAttack : ServerUpdate
 Move : BoardIndex -> ServerUpdate
 SkillInitiation : Nat -> ServerUpdate
 SkillSelection : Nat -> ServerUpdate {-this currently wrong-}
 Revive : Vect 9 Bool -> ServerUpdate
 DrawCard : Nat -> ServerUpdate {-The natural number is the ID of the card in some representation. For now this should be stored in Idris, though Ur/Web could also participate eventually by storing a database.-}

public export
ServerUpdateWrapper : Type
ServerUpdateWrapper = (ServerUpdate, String)
