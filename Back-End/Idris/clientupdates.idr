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
                  | Kill Nat String String {-board index, player token, opponent token-}
                  | DeployCardRequest String
                  | RequestDrawHand (String,String)
                  | RequestDrawSoul (String,String)
                  | SendSpawnToDiscard String String
                        

                  {-
                  
                  | SetStat statblah Nat String String
                  | Set
  -}
                  
                                                  
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
