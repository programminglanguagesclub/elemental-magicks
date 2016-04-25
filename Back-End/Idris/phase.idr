module Phase
import clientupdates

public export
data Phase = DrawPhase
           | SpawnPhase
           | SpellPhase
           | RemovalPhase
           | StartPhase
           | EngagementPhase
           | EndPhase
           | RevivalPhase
           | DeploymentPhase
public export
nextPhase : Phase -> (Phase,ClientUpdate) {-am I ever going to use this function???? Perhaps this should return the appropriate client update too?-}
nextPhase DrawPhase = (SpawnPhase,DrawPhaseToSpawnPhase)
nextPhase SpawnPhase = (SpellPhase,SpawnPhaseToSpellPhase)
nextPhase SpellPhase = (RemovalPhase,SpellPhaseToRemovalPhase)
nextPhase RemovalPhase = (StartPhase,RemovalPhaseToStartPhase)
nextPhase StartPhase = (EngagementPhase,StartPhaseToEngagementPhase)
nextPhase EngagementPhase = (EndPhase,EngagementPhaseToEndPhase)
nextPhase EndPhase = (RevivalPhase,EndPhaseToRevivalPhase)
nextPhase RevivalPhase = (DeploymentPhase,RevivalPhaseToDeploymentPhase)
nextPhase DeploymentPhase = (SpawnPhase,DeploymentPhaseToSpawnPhase)

{-I believe the relative order of Revival and Deployment does not matter, but I find this version more ergonomic-}


