module Base.Phase
import Base.Clientupdates
import Data.Vect
import Data.Fin
import Base.Player


public export
data PhaseCycle
 = SpawnPhase
 | SpellPhase
 | RemovalPhase
 | StartPhase {-Let's damage soul at the start of this phase!-}
 | EngagementPhase
 | EndPhase
 | RevivalPhase
 | DeploymentPhase

public export
data Phase
 = DrawPhase DrawPlayer DrawPlayer (Fin 60)
 | MkPhaseCycle PhaseCycle Player Player




 {-
 I can put into the constructor to drawplayer a counter which keeps track of the number of
  soul cards that have been added (or are left to add), and then only allow drawplayer to be constructed via accessing those constructors.
 -}

numNothing : Vect n (Maybe a) -> Nat
numNothing = ?hole


{-This does not perform any actions which should be performed at the start or end of a phase. It just performs the transition... for now at least... -}

public export
nextPhase : PhaseCycle -> PhaseCycle
nextPhase SpawnPhase = SpellPhase
nextPhase SpellPhase = RemovalPhase
nextPhase RemovalPhase = StartPhase
nextPhase StartPhase = EngagementPhase
nextPhase EngagementPhase = EndPhase
nextPhase EndPhase = RevivalPhase
nextPhase RevivalPhase = DeploymentPhase
nextPhase DeploymentPhase = SpawnPhase

{-
public export
nextPhase : Phase -> (Phase,ClientUpdate)
nextPhase (DrawPhase playerA playerB) = ((PhaseCycle SpawnPhase) ?hole ?hole,DrawPhaseToSpawnPhase)
nextPhase (SpawnPhase playerA playerB) = ((Phase Cycle SpellPhase ?hole ?hole,SpawnPhaseToSpellPhase)
nextPhase (SpellPhase playerA playerB) = (RemovalPhase ?hole ?hole,SpellPhaseToRemovalPhase)
nextPhase (RemovalPhase playerA playerB) = (StartPhase ?hole ?hole,RemovalPhaseToStartPhase)
nextPhase (StartPhase playerA playerB) = (EngagementPhase ?hole ?hole,StartPhaseToEngagementPhase)
nextPhase (EngagementPhase playerA playerB) = (EndPhase ?hole ?hole,EngagementPhaseToEndPhase)
nextPhase (EndPhase playerA playerB) = (RevivalPhase ?hole ?hole,EndPhaseToRevivalPhase)
nextPhase (RevivalPhase playerA playerB) = (DeploymentPhase ?hole ?hole,RevivalPhaseToDeploymentPhase)
nextPhase (DeploymentPhase playerA playerB) = (SpawnPhase ?hole ?hole,DeploymentPhaseToSpawnPhase)

-}

{-I believe the relative order of Revival and Deployment does not matter, but I find this version more ergonomic-}


