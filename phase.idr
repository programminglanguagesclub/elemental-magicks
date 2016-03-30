module Phase

public export data Phase = DrawPhase
           | SpawnPhase
           | SpellPhase
           | RemovalPhase
           | StartPhase
           | EngagementPhase
           | EndPhase
           | RevivalPhase
public export nextPhase : Phase -> Phase
nextPhase DrawPhase = SpawnPhase
nextPhase SpawnPhase = SpellPhase
nextPhase SpellPhase = RemovalPhase
nextPhase RemovalPhase = StartPhase
nextPhase StartPhase = EngagementPhase
nextPhase EngagementPhase = EndPhase
nextPhase EndPhase = RevivalPhase
nextPhase RevivalPhase = SpawnPhase

