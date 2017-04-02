module Base.Phase
import Base.Clientupdates

public export
{-
data Phase = DrawPhase DrawPlayer {-dependent pair....  remember to take player out of game...-}
           | SpawnPhase NormalPlayer
           | SpellPhase NormalPlayer
           | RemovalPhase NormalPlayer
           | StartPhase NormalPlayer {-Let's damage soul at the start of this phase!-}
           | EngagementPhase NormalPlayer
           | EndPhase NormalPlayer
           | RevivalPhase NormalPlayer
           | DeploymentPhase NormalPlayer
           -}

data Phase : Type where
 DrawPhase : Phase
 SpawnPhase : Phase
 SpellPhase : Phase
 RemovalPhase : Phase
 StartPhase : Phase
 EngagementPhase : Phase
 EndPhase : Phase
 RevivalPhase : Phase
 DeploymentPhase : Phase

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


