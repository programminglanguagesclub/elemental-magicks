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
nextPhase : Phase -> Phase {-am I ever going to use this function???? Perhaps this should return the appropriate client update too?-}
nextPhase DrawPhase = SpawnPhase
nextPhase SpawnPhase = SpellPhase
nextPhase SpellPhase = RemovalPhase
nextPhase RemovalPhase = StartPhase
nextPhase StartPhase = EngagementPhase
nextPhase EngagementPhase = EndPhase
nextPhase EndPhase = RevivalPhase
nextPhase RevivalPhase = DeploymentPhase
nextPhase DeploymentPhase = SpawnPhase

{-I believe the relative order of Revival and Deployment does not matter, but I find this version more ergonomic-}



