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
                  | SkillSelection (List (Fin 9)) (List (Fin 9)) (List Nat) (List Nat) (List Nat) (List Nat) String {-no requirement of uniqueness at type level currently...-}
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
getRevivePositions : String -> Maybe (Vect 9 Bool)

{- parseBounded : (lower : Integer) -> (upper : Integer) -> String -> Maybe (Bounded lower upper) -}
parseFin : (n : Nat) -> String -> Maybe (Fin n)
parseListFin : (n : Nat) -> String -> Maybe (List (Fin n))
parseListNat : String -> Maybe (List Nat)


{-definitely want to use monads here-}
generateServerUpdate : MarshalledServerUpdate -> Maybe ServerUpdate
generateServerUpdate marshalledServerUpdate with (type marshalledServerUpdate)
  | "spawnCard" = do rawSchools <- getField (info marshalledServerUpdate) "schools"
                     schools <- getSchools rawSchools
                     rawIndex <- getField (info marshalledServerUpdate) "index"
                     index <- parsePositive {a=Nat} rawIndex
                     return (SpawnCard schools index $ player marshalledServerUpdate)
  | "skip" = do rawSchools <- getField (info marshalledServerUpdate) "schools"
                schools <- getSchools rawSchools
                return (Skip schools $ player marshalledServerUpdate)
  | "deployCard" = do rawIndex <- getField (info marshalledServerUpdate) "index"
                      index <- parseFin 9 rawIndex
                      return (DeployCard index $ player marshalledServerUpdate)
  | "attackRow" = do rawRow <- getField (info marshalledServerUpdate) "row"
                     row <- parseFin 3 rawRow
                     return (AttackRow row $ player marshalledServerUpdate)
  | "rest" = do return (Rest $ player marshalledServerUpdate)
  | "directAttack" = do return (DirectAttack $ player marshalledServerUpdate)
  | "move" = do rawTo <- getField (info marshalledServerUpdate) "to"
                to <- parseFin 9 rawTo
                return (Move to $ player marshalledServerUpdate)
  | "skillInitiation" = do rawIndex <- getField (info marshalledServerUpdate) "index"
                           index <- parsePositive {a=Nat} rawIndex
                           return (SkillInitiation index $ player marshalledServerUpdate)
  | "skillSelection" = do rawFriendlyBoard <- getField (info marshalledServerUpdate) "friendlyBoard"
                          friendlyBoard <- parseListFin 9 rawFriendlyBoard
                          rawEnemyBoard <- getField (info marshalledServerUpdate) "enemyBoard"
                          enemyBoard <- parseListFin 9 rawEnemyBoard
                          rawFriendlyHand <- getField (info marshalledServerUpdate) "friendlyHand"
                          friendlyHand <- parseListNat rawFriendlyHand
                          rawEnemyHand <- getField (info marshalledServerUpdate) "enemyHand"
                          enemyHand <- parseListNat rawEnemyHand
                          rawFriendlyGraveyard <- getField (info marshalledServerUpdate) "friendlyGraveyard"
                          friendlyGraveyard <- parseListNat rawFriendlyGraveyard
                          rawEnemyGraveyard <- getField (info marshalledServerUpdate) "enemyGraveyard"
                          enemyGraveyard <- parseListNat rawEnemyGraveyard
                          return (SkillSelection friendlyBoard enemyBoard friendlyHand enemyHand friendlyGraveyard enemyGraveyard $ player marshalledServerUpdate)
  | "revive" = do rawPositions <- getField (info marshalledServerUpdate) "positions"
                  positions <- getRevivePositions rawPositions
                  return (Revive positions $ player marshalledServerUpdate)
  | "drawCard" = do rawId <- getField (info marshalledServerUpdate) "id"
                    id <- parsePositive {a=Nat} rawId
                    return (DrawCard id $ player marshalledServerUpdate)




{-some more monad fun to do in these help functions-}
removeSpaces : String -> String {-does not remove all whitespace-}
removeSpaces "" = ""
removeSpaces s with (strHead s)
  | ' ' = removeSpaces (strTail s)
  | x = (singleton x) ++ (removeSpaces (strTail s))
shedBrackets : String -> Maybe String
shedBrackets s = ?hole


generateRawKeyValueList : String -> List String
generateRawKeyValueList s = split (==',') s

generateParsedKeyValue : String -> Maybe (String,String)
generateParsedKeyValue s = let ss = split (==':') s in case ss of
                                                            [s1,s2] => Just (s1,s2)
                                                            _ => Nothing


generateParsedKeyValueList : List String -> Maybe (List (String,String))
generateParsedKeyValueList [] = Just []
generateParsedKeyValueList (x::xs) = do firstParsed <- generateParsedKeyValue x
                                        remainingParsed <- generateParsedKeyValueList xs
                                        return (firstParsed :: remainingParsed)

extractField : String -> List (String,String) -> Maybe String
extractField _ [] = Nothing
extractField name ((key,val)::xs) with (name == key)
  | True = Just val
  | False = extractField name xs

removeField : String -> List (String,String) -> List (String,String)
removeField _ [] = []
removeField name ((key,value)::xs) with (name == key)
  | True = xs
  | False = (key,value) :: (removeField name xs)

extractAndRemoveField : String -> List (String,String) -> Maybe (List (String,String), String)
extractAndRemoveField name pairs = do val <- extractField name pairs
                                      return (removeField name pairs, val)


marshallJson : String -> Maybe MarshalledServerUpdate
marshallJson json = do cleanedJson <- shedBrackets $ removeSpaces json
                       keyValueList <- generateParsedKeyValueList $ generateRawKeyValueList cleanedJson
                       (keyValueList',id) <- extractAndRemoveField "player" keyValueList
                       (keyValueList'',updateType) <- extractAndRemoveField "updateType" keyValueList' 
                       return (MkMarshalledServerUpdate updateType id keyValueList'')


parseJson : String -> Maybe ServerUpdate
parseJson json = do marshalledJson <- marshallJson json
                    serverUpdate <- generateServerUpdate marshalledJson
                    return serverUpdate


