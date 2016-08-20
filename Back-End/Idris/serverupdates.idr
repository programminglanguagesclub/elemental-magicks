module ServerUpdates

import Data.Vect
import Data.Fin
import Data.So
import Data.String
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
data ServerUpdate = SpawnCard (Vect 6 (Bounded 0 9)) Nat String
                  | Skip (Vect 6 (Bounded 0 9)) String
                  | DeployCard (Fin 9) String
                  | AttackRow (Fin 3) String
                  | Rest String
                  | DirectAttack String
                  | Move (Fin 9) String
                  | SkillInitiation Nat String
                  | SkillSelection (List Nat, List Nat, List Nat, List Nat, List Nat, List Nat) String
                  | Revive (Vect 9 Bool) String
                  | DrawCard Nat String {-The natural number is the ID of the card in some representation. For now this should be stored in Idris, though Ur/Web could also participate eventually by storing a database.-}

record MarshalledServerUpdate where
  constructor MkMarshalledServerUpdate
  type : String
  player : String
  info : List (String,String)


getField : List(String,String) -> String -> Maybe String
getField [] _ = Nothing
getField ((k,v)::xs) x = if (k==x) then Just v else getField xs x

getSchools : String -> Maybe (Vect 6 (Bounded 0 9))




{-definitely want to use monads here-}
generateServerUpdate : MarshalledServerUpdate -> Maybe ServerUpdate
generateServerUpdate marshalledServerUpdate with (type marshalledServerUpdate)
  | "spawnCard" = case getField (info marshalledServerUpdate) "schools" of
                       Nothing => Nothing
                       Just rawSchools => case getSchools rawSchools of
                                               Nothing => Nothing
                                               Just schools => case getField (info marshalledServerUpdate) "index" of
                                                                    Nothing => Nothing
                                                                    Just rawIndex => case parsePositive {a=Nat} rawIndex of
                                                                                          Nothing => Nothing
                                                                                          Just index => Just $ SpawnCard schools index $ player marshalledServerUpdate
  | "skip" = ?hole
  | "deployCard" = ?hole
  | "attackRow" = ?hole
  | "rest" = ?hole
  | "directAttack" = ?hole
  | "move" = ?hole
  | "skillInitiation" = ?hole
  | "skillSelection" = ?hole
  | "revive" = ?hole
  | "drawCard" = ?hole
