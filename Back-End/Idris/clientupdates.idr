module Clientupdates
import Data.Fin
import Data.Vect
import preliminaries
import objects_basic
import bounded
import bounded_then_integer
import integer_then_bounded
public export
data ClientUpdate = GameLogicError
                  | RoundTerminated {-Currently don't progress to next round, and also don't distinguish players quite yet...-}
                  | DrawPhaseToSpawnPhase
                  | SpawnPhaseToSpellPhase
                  | SpellPhaseToRemovalPhase
                  | RemovalPhaseToStartPhase
                  | StartPhaseToEngagementPhase
                  | EngagementPhaseToEndPhase
                  | EndPhaseToRevivalPhase
                  | RevivalPhaseToDeploymentPhase
                  | DeploymentPhaseToSpawnPhase
                  | InvalidMove String String {-error message ; player id-}
                  | Kill (Fin 9) String {-board index, player id-}
                  | DeployCard (Fin 9) String
                  | DrawHand Nat String
                  | DrawSoul Nat (Fin 5) String
                  | SendSpawnToDiscard String
                  | MoveUnit (Fin 9) (Fin 9) String      
                  | UpdateThoughts (Bounded 0 Preliminaries.absoluteUpperBound) String
                  | UpdateSchools (Vect 6 (Bounded 0 9)) String
                  | LoseSoulPoint String {- for now you only lose one at a time -}
                  | SendBoardToGraveyard (Fin 9) String
                  | SetStat String String (Fin 9) String {-stat name, marshalled stat value, board index, player Id -}
                  | SpawnCard Nat String
                  | PlayerTurn String



{-if it's expensive to write on pipes, some of this code could be moved into the Ur/Web-}
public export
getCardName : Nat -> Maybe String
record MarshalledClientUpdate where
 constructor MkMarshalledClientUpdate 
 type : String
 info : List (String, String) {-name of field, value of field.. for now no uniqueness guarantee at the type level-}
augment : MarshalledClientUpdate -> Bool -> MarshalledClientUpdate
augment marshalledClientUpdate b = record {info = ("player",if b then "player" else "opponent") :: (info marshalledClientUpdate)} marshalledClientUpdate
marshallClientUpdate : ClientUpdate -> String -> Maybe MarshalledClientUpdate {-nothing if the user should not be receiving this update-}
marshallClientUpdate GameLogicError _ = Just $ MkMarshalledClientUpdate "gameLogicError" []
marshallClientUpdate RoundTerminated _ = Just $ MkMarshalledClientUpdate "roundTerminated" [] {-include data about the next round?-}
marshallClientUpdate DrawPhaseToSpawnPhase _ = Just $ MkMarshalledClientUpdate "drawPhaseToSpawnPhase" []
marshallClientUpdate SpawnPhaseToSpellPhase _ = Just $ MkMarshalledClientUpdate "spawnPhaseToSpellPhase" []
marshallClientUpdate SpellPhaseToRemovalPhase _ = Just $ MkMarshalledClientUpdate "spellPhaseToRemovalphase" []
marshallClientUpdate RemovalPhaseToStartPhase _ = Just $ MkMarshalledClientUpdate "removalPhaseToStartPhase" []
marshallClientUpdate StartPhaseToEngagementPhase _ = Just $ MkMarshalledClientUpdate "startPhaseToEngagementPhase" []
marshallClientUpdate EngagementPhaseToEndPhase _ = Just $ MkMarshalledClientUpdate "engagementPhaseToEndPhase" []
marshallClientUpdate EndPhaseToRevivalPhase _ = Just $ MkMarshalledClientUpdate "endPhaseToRevivalPhase" []
marshallClientUpdate RevivalPhaseToDeploymentPhase _ = Just $ MkMarshalledClientUpdate "revivalPhaseToDeploymentPhase" []
marshallClientUpdate DeploymentPhaseToSpawnPhase _ = Just $ MkMarshalledClientUpdate "deploymentPhaseToSpawnPhase" []
marshallClientUpdate (InvalidMove message playerId) id with (playerId == id)
  | False = Nothing
  | True = Just $ MkMarshalledClientUpdate "invalidMove" [("description",message)]
marshallClientUpdate (Kill boardIndex playerId) id = Just $ augment (MkMarshalledClientUpdate "kill" [("index",show $ finToNat boardIndex)]) (playerId == id)
marshallClientUpdate (DeployCard boardIndex playerId) id = Just $ augment (MkMarshalledClientUpdate "deployCard" [("index",show $ finToNat boardIndex)]) (playerId == id)
{-message currently 0 indexes the board-}
marshallClientUpdate (DrawHand cardId playerId) id =
  do cardName <- getCardName cardId
     return (augment (MkMarshalledClientUpdate "drawHandCard" [("name",cardName)]) (playerId == id))
  {-THIS is actually going to have a lot more data than the name: essentially all of the data of the card-}
marshallClientUpdate (DrawSoul cardId soulIndex playerId) id =
  do cardName <- getCardName cardId
     return (augment (MkMarshalledClientUpdate "drawSoulCard" [("name",cardName),("index",show $ finToNat soulIndex)]) (playerId == id))
marshallClientUpdate (SendSpawnToDiscard playerId) id = Just $ augment (MkMarshalledClientUpdate "sendSpawnToDiscard" []) (playerId == id)
marshallClientUpdate (MoveUnit from to playerId) id = Just $ augment (MkMarshalledClientUpdate "moveUnit" [("from",show $ finToNat from),("to",show $ finToNat to)]) (playerId == id)
marshallClientUpdate (UpdateThoughts val playerId) id = Just $ augment (MkMarshalledClientUpdate "updateThoughts" [("val",show $ extractBounded val)]) (playerId == id)
marshallClientUpdate (UpdateSchools schools playerId) id =
 Just $ augment (MkMarshalledClientUpdate "updateSchools" (toList $ zipWith (\x,y => (x,show $ extractBounded y)) ["earth","fire","water","air","spirit","void"] schools)) (playerId == id)
marshallClientUpdate (LoseSoulPoint playerId) id = Just $ augment (MkMarshalledClientUpdate "loseSoulPoint" []) (playerId == id)
marshallClientUpdate (SendBoardToGraveyard boardIndex playerId) id = Just $ augment (MkMarshalledClientUpdate "sendBoardToGraveyard" [("boardIndex",show $ finToInteger boardIndex)]) (playerId == id)
marshallClientUpdate (SetStat stat val boardIndex playerId) id = Just $ augment (MkMarshalledClientUpdate "setStat" [("stat",stat),("val",val),("index",show $ finToNat boardIndex)]) (playerId == id)
marshallClientUpdate (SpawnCard handIndex playerId) id = Just $ augment (MkMarshalledClientUpdate "spawnCard" [("index",show handIndex)]) (playerId == id)
marshallClientUpdate (PlayerTurn playerId) id = Just $ augment (MkMarshalledClientUpdate "playerTurn" []) (playerId == id)
serializeInfo : List (String,String) -> String
serializeInfo [] = ""
serializeInfo ((k,v)::xs) = "," ++ k ++ ":" ++ v ++ (serializeInfo xs)
serializeMarshalled : MarshalledClientUpdate -> String
serializeMarshalled marshalledClientUpdate = "{updateType:" ++ (type marshalledClientUpdate) ++ (serializeInfo (info marshalledClientUpdate)) ++ "}" {-player token added by ur/web-}
serialize : ClientUpdate -> String -> Maybe String
serialize clientUpdate playerId = do marshalledClientUpdate <- marshallClientUpdate clientUpdate playerId
                                     return (serializeMarshalled marshalledClientUpdate)
