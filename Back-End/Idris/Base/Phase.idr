module Base.Phase
import Base.Preliminaries
import Base.Clientupdates
import Data.Vect
import Data.Fin
import Base.Player
import Base.Card


public export
data Phase
 = SpawnPhase
 | SpellPhase
 | RemovalPhase
 | StartPhase {-Let's damage soul at the start of this phase!-}
 | EngagementPhase (FieldedMonster, Fin 9, WhichPlayer)
 | EndPhase
 | RevivalPhase
 | DeploymentPhase

public export
nextPhase : Phase -> Phase
nextPhase SpawnPhase = SpellPhase
nextPhase SpellPhase = RemovalPhase
nextPhase RemovalPhase = StartPhase
nextPhase StartPhase = EngagementPhase ?hole
nextPhase EngagementPhase = EndPhase
nextPhase EndPhase = RevivalPhase
nextPhase RevivalPhase = DeploymentPhase
nextPhase DeploymentPhase = SpawnPhase


-- after draft phase comes spawn.

