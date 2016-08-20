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
                  | DrawSoul Nat String
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
 type, description, data : String


marshallClientUpdate : ClientUpdate -> String -> Maybe MarshalledClientUpdate {-nothing if the user should not be receiving this update-}
marshallClientUpdate GameLogicError _ = Just $ MkMarshalledClientUpdate "gameLogicError" "Game Logic Error" ""
marshallClientUpdate RoundTerminated _ = Just $ MkMarshalledClientUpdate "roundTerminated" "Round Terminated" "" {-include data about the next round?-}
marshallClientUpdate DrawPhaseToSpawnPhase _ = Just $ MkMarshalledClientUpdate "drawPhaseToSpawnPhase" "Draw Phase Ends - Spawn Phase Begins" ""
marshallClientUpdate SpawnPhaseToSpellPhase _ = Just $ "spawnPhase""Spawn Phase Ends - Spell Phase Begins"
marshallClientUpdate SpellPhaseToRemovalPhase _ = Just "Spell Phase Ends - Removal Phase Begins"
marshallClientUpdate RemovalPhaseToStartPhase _ = Just "Removal Phase Ends - Start Phase Begins"
marshallClientUpdate StartPhaseToEngagementPhase _ = Just "Start Phase Ends - Engagement Phase Begins"
marshallClientUpdate EngagementPhaseToEndPhase _ = Just "Engagement Phase Ends - End Phase Begins"
marshallClientUpdate EndPhaseToRevivalPhase _ = Just "End Phase Ends - Revival Phase Begins"
marshallClientUpdate RevivalPhaseToDeploymentPhase _ = Just "Revival Phase Ends - Deployment Phase Begins"
marshallClientUpdate DeploymentPhaseToSpawnPhase _ = Just "Deployment Phase Ends - Spawn Phase Begins"
marshallClientUpdate (InvalidMove message playerId) id with (playerId == id)
  | False = Nothing
  | True = Just ("Invalid Move: " ++ message)
marshallClientUpdate (Kill boardIndex playerId) id = ?hole
marshallClientUpdate (DeployCard boardIndex playerId) id with (playerId == id)
  | False = Just("Enemy card deployed to position " $ show boardIndex)
  | True = Just("Card deployed to position " ++ $ show boardIndex) {-message currently 0 indexes the board-}
marshallClientUpdate (DrawHand cardId playerId) id with (playerId == id)
  | False = Just("Enemy drew card"
  | True = Just("") {-need to have some other -}






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
