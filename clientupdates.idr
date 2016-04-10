module Clientupdates

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
                  | InvalidMove

public export
data ClientUpdateWrapper = SendToOnePlayer (ClientUpdate, String)
                         | SendToBothPlayers ClientUpdate


{- mess
public export
transformClientUpdate : ClientUpdate -> ClientUpdate
transformClientUpdate clientUpdate with (clientUpdate)
 | SendToOnePlayer (update
-}
