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
getCardName : Nat -> String
record MarshalledClientUpdate where
 constructor MkMarshalledClientUpdate 
 type : String
 info : List (String, String) {-name of field, value of field.. for now no uniqueness guarantee at the type level-}

augmentMarshalledClientUpdate : MarshalledClientUpdate -> Bool -> MarshalledClientUpdate
augmentMarshalledClientUpdate marshalledClientUpdate b = record {info = ("player",if b then "player" else "opponent") :: (info marshalledClientUpdate)} marshalledClientUpdate
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
marshallClientUpdate (Kill boardIndex playerId) id = Just $ MkMarshalledClientUpdate "kill" []
marshallClientUpdate (DeployCard boardIndex playerId) id with (playerId == id)
  | False = Just $ MkMarshalledClientUpdate "deployCard" [("index",show $ finToInteger boardIndex),("player","opponent")]
  | True = Just $ MkMarshalledClientUpdate "deployCard" [("index",show $ finToInteger boardIndex),("player","player")] {-message currently 0 indexes the board-}
marshallClientUpdate (DrawHand cardId playerId) id with (playerId == id)
  | False = Just $ MkMarshalledClientUpdate "drawHandCard" [("name",getCardName cardId),("player","opponent")]
  {-THIS is actually going to have a lot more data than the name: essentially all of the data of the card-}
  | True = Just $ MkMarshalledClientUpdate "drawHandCard" [("name",getCardName cardId),("player","player")] {-need to have some other -}
marshallClientUpdate (DrawSoul cardId soulIndex playerId) id with (playerId == id)
  | False = Just $ MkMarshalledClientUpdate "drawSoulCard" [("name",getCardName cardId),("index",show $ finToInteger soulIndex),("player","opponent")]
  | True = Just $ MkMarshalledClientUpdate "drawSoulCard" [("name",getCardName cardId),("index",show $ finToInteger soulIndex),("player","player")]
marshallClientUpdate (SendSpawnToDiscard playerId) id with (playerId == id)
  | False = Just $ MkMarshalledClientUpdate "sendSpawnToDiscard" [("player","opponent")]
  | True = Just $ MkMarshalledClientUpdate "sendSpawnToDiscard" [("player","player")]
marshallClientUpdate (MoveUnit from to playerId) id with (playerId == id)
  | False = Just $ MkMarshalledClientUpdate "moveUnit" [("player","opponent"),("from",show $ finToInteger from),("to",show $ finToInteger to)]
  | True = Just $ MkMarshalledClientUpdate "moveUnit" [("player","player"),("from",show $ finToInteger from),("to",show $ finToInteger to)]
marshallClientUpdate (UpdateThoughts val playerId) id with (playerId == id)
  | False = Just $ MkMarshalledClientUpdate "updateThoughts" [("player","opponent"),("val",show $ extractBounded val)]
  | True = Just $ MkMarshalledClientUpdate "updateThoughts" [("player","player"),("val",show $ extractBounded val)]
marshallClientUpdate (UpdateSchools [earth,fire,water,air,spirit,void'] playerId) id with (playerId == id)
  | False = Just $ MkMarshalledClientUpdate "updateSchools" [("player","opponent"),("earth",show $ extractBounded earth), ("fire",show $ extractBounded fire), ("water", show $ extractBounded water), ("air", show $ extractBounded air),("spirit", show $ extractBounded spirit),("void",show $ extractBounded void')]
  | True = Just $ MkMarshalledClientUpdate "updateSchools" [("player","player"),("earth",show $ extractBounded earth),("fire", show $ extractBounded fire),("water", show $ extractBounded water), ("air", show $ extractBounded air),("spirit",show $ extractBounded spirit),("void", show $ extractBounded void')]
marshallClientUpdate (LoseSoulPoint playerId) id with (playerId == id)
  | False = Just $ MkMarshalledClientUpdate "loseSoulPoint" [("player","opponent")]
  | True = Just $ MkMarshalledClientUpdate "loseSoulPoint" [("player","player")]
marshallClientUpdate (SendBoardToGraveyard boardIndex playerId) id with (playerId == id)
  | False = Just $ MkMarshalledClientUpdate "sendBoardToGraveyard" [("player","opponent"),("boardIndex",show $ finToInteger boardIndex)]
  | True = Just $ MkMarshalledClientUpdate "sendBoardToGraveyard" [("player","player"),("boardIndex",show $ finToInteger boardIndex)]
marshallClientUpdate (SetStat stat val boardIndex playerId) id = ?hole
marshallClientUpdate (SpawnCard handIndex playerId) id = ?hole




{- more cases.. -}









{-
public export
data MarshalledClientUpdate = SendToOnePlayer (ClientUpdate, String)
                         | SendToBothPlayers ClientUpdate
-}

{- mess
public export
transformClientUpdate : ClientUpdate -> ClientUpdate
transformClientUpdate clientUpdate with (clientUpdate)
 | SendToOnePlayer (update
-}

{-should use a typeclass for sendToOnePlayer, etc?-}
{-
public export
serializeUpdateWrapper : ClientUpdateWrapper -> (String, String) {-update, player_token it's for-}
serializeUpdateWrapper (SendToOnePlayer (foo,bar)) = ("","")
serializeUpdateWrapper _ = ?gg
-}
