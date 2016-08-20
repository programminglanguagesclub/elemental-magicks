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
                  | Kill Nat String String {-board index, player token, opponent token-}
                  | DeployCardRequest String
                  | DrawHand Nat String String
                  | DrawSoul Nat String String
                  | SendSpawnToDiscard String String
                  | MoveUnit (Fin 9) (Fin 9) String String      
                  | UpdateThoughts (Bounded 0 Preliminaries.absoluteUpperBound) String String
                  | UpdateSchools (Vect 6 (Bounded 0 9)) String String
                  | LoseSoulPoint String String {- for now you only lose one at a time -}
                  | SendBoardToGraveyard (Fin 9) String String
                  | SetStat String String Nat String String {-stat name, marshalled stat value, board index, player Id, opponent Id -}
                  | SpawnCard Nat String String


marshallClientUpdate : ClientUpdate -> String -> Maybe String {-nothing if the user should not be receiving this update-}
marshallClientUpdate GameLogicError _ = Just "Game Logic Error"
marshallClientUpdate RoundTerminated _ = Just "Round Terminated"
marshallClientUpdate DrawPhaseToSpawnPhase _ = Just "Draw Phase Ends - Spawn Phase Begins"
marshallClientUpdate SpawnPhaseToSpellPhase _ = Just "Spawn Phase Ends - Spell Phase Begins"
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
marshallClientUpdate (Kill boardIndex playerId opponentId) id = ?hole {-I don't really need the opponent Id too do I? I already know the player in question is one of the players in the game.... -}
                                                  
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
