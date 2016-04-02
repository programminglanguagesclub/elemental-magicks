module Phase

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
nextPhase : Phase -> Phase
nextPhase DrawPhase = SpawnPhase
nextPhase SpawnPhase = SpellPhase
nextPhase SpellPhase = RemovalPhase
nextPhase RemovalPhase = StartPhase
nextPhase StartPhase = EngagementPhase
nextPhase EngagementPhase = EndPhase
nextPhase EndPhase = RevivalPhase
nextPhase RevivalPhase = DeploymentPhase
nextPhase DeploymentPhase = SpawnPhase





