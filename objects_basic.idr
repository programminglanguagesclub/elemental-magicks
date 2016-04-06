module Objects_basic

import Data.Vect
import Data.So
import preliminaries

public export data Aliveness = Alive | DeadFresh | DeadStale

public export
record BasicMonster where
 constructor MkBasicMonster
 attack : TemporaryPermanentBase Attack
 defense : TemporaryPermanentBase Defense
 speed : TemporaryPermanentBase Speed
 range : TemporaryPermanentBase Range
 level : TemporaryPermanentBase Level
 aliveness : Aliveness

public export
record BasicSpell where
 constructor MkBasicSpell
 level : Level

public export
data BasicCard = BasicSpellCard BasicSpell | BasicMonsterCard BasicMonster

{-
syntax repeat3 [val] = ((val ** Oh),(val ** Oh),(val ** Oh))
syntax basic monster [attack] [defense] [speed] [range] [level] = MkBasicMonster (repeat3 attack) (repeat3 defense) (repeat3 speed) (repeat3 range) (repeat3 level) Alive Nothing False Nothing False Nothing False Nothing False [] False
syntax basic spell [level] = MkBasicSpell level False
-}


{-
mutant_pig : BasicMonster
mutant_pig = basic monster 20 0 2 1 3
-}

{-
foo : Card
foo = BasicMonsterCard 453 mutant_pig
-}

