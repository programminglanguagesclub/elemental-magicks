module Base.Phase
import Base.Clientupdates
import Data.Vect
import Base.Player

public export

data Phase
 = DrawPhase DrawPlayer DrawPlayer {-dependent pair....  remember to take player out of game record...-}
 | SpawnPhase Player Player
 | SpellPhase Player Player
 | RemovalPhase Player Player
 | StartPhase Player Player {-Let's damage soul at the start of this phase!-}
 | EngagementPhase Player Player
 | EndPhase Player Player
 | RevivalPhase Player Player
 | DeploymentPhase Player Player
 

 {-
 I can put into the constructor to drawplayer a counter which keeps track of the number of
  soul cards that have been added (or are left to add), and then only allow drawplayer to be constructed via accessing those constructors.
 -}
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
nextPhase (DrawPhase playerA playerB) = (SpawnPhase ?hole ?hole,DrawPhaseToSpawnPhase)
nextPhase (SpawnPhase playerA playerB) = (SpellPhase ?hole ?hole,SpawnPhaseToSpellPhase)
nextPhase (SpellPhase playerA playerB) = (RemovalPhase ?hole ?hole,SpellPhaseToRemovalPhase)
nextPhase (RemovalPhase playerA playerB) = (StartPhase ?hole ?hole,RemovalPhaseToStartPhase)
nextPhase (StartPhase playerA playerB) = (EngagementPhase ?hole ?hole,StartPhaseToEngagementPhase)
nextPhase (EngagementPhase playerA playerB) = (EndPhase ?hole ?hole,EngagementPhaseToEndPhase)
nextPhase (EndPhase playerA playerB) = (RevivalPhase ?hole ?hole,EndPhaseToRevivalPhase)
nextPhase (RevivalPhase playerA playerB) = (DeploymentPhase ?hole ?hole,RevivalPhaseToDeploymentPhase)
nextPhase (DeploymentPhase playerA playerB) = (SpawnPhase ?hole ?hole,DeploymentPhaseToSpawnPhase)

{-I believe the relative order of Revival and Deployment does not matter, but I find this version more ergonomic-}


