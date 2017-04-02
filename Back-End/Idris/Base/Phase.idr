module Base.Phase
import Base.Clientupdates
import Data.Vect
import Base.Player

public export

data Phase
 = DrawPhase DrawPlayer {-dependent pair....  remember to take player out of game...-}
 | SpawnPhase NormalPlayer
 | SpellPhase NormalPlayer
 | RemovalPhase NormalPlayer
 | StartPhase NormalPlayer {-Let's damage soul at the start of this phase!-}
 | EngagementPhase NormalPlayer
 | EndPhase NormalPlayer
 | RevivalPhase NormalPlayer
 | DeploymentPhase NormalPlayer
           
{-
data Phase : Type where
 DrawPhase : DrawPlayer -> Phase
 SpawnPhase : Phase
 SpellPhase : Phase
 RemovalPhase : Phase
 StartPhase : Phase
 EngagementPhase : Phase
 EndPhase : Phase
 RevivalPhase : Phase
 DeploymentPhase : Phase
 -}

numNothing : Vect n (Maybe a) -> Nat
numNothing = ?hole


public export
nextPhase : Phase -> (Phase,ClientUpdate) {-am I ever going to use this function???? Perhaps this should return the appropriate client update too?-}
nextPhase (DrawPhase drawPlayer) = (SpawnPhase ?hole,DrawPhaseToSpawnPhase)
nextPhase (SpawnPhase normalPlayer) = (SpellPhase ?hole,SpawnPhaseToSpellPhase)
nextPhase (SpellPhase normalPlayer) = (RemovalPhase ?hole,SpellPhaseToRemovalPhase)
nextPhase (RemovalPhase normalPlayer) = (StartPhase ?hole,RemovalPhaseToStartPhase)
nextPhase (StartPhase normalPlayer) = (EngagementPhase ?hole,StartPhaseToEngagementPhase)
nextPhase (EngagementPhase normalPlayer) = (EndPhase ?hole,EngagementPhaseToEndPhase)
nextPhase (EndPhase normalPlayer) = (RevivalPhase ?hole,EndPhaseToRevivalPhase)
nextPhase (RevivalPhase normalPlayer) = (DeploymentPhase ?hole,RevivalPhaseToDeploymentPhase)
nextPhase (DeploymentPhase normalPlayer) = (SpawnPhase ?hole,DeploymentPhaseToSpawnPhase)

{-I believe the relative order of Revival and Deployment does not matter, but I find this version more ergonomic-}


