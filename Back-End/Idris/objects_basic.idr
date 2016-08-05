module Objects_basic

import Data.Vect
import Data.So
import preliminaries
import bounded
import bounded_then_integer
import integer_then_bounded

public export data Aliveness = Alive | DeadFresh | DeadStale

public export {-eventually make multischool required to be unique at the type level-}
data MonsterSchools = NoSchools
                    | OneSchool School
                    | TwoSchools School School


public export
record BasicMonster where
 constructor MkBasicMonster
 permanentId : Nat {-equivalence classes of cards. Used for revival-}
 temporaryId : Nat {-Id of a particular card for the game-}
 {-schools : List Nat {-Vector of elements 0~5 of length 0, 1 or 2, with distinct elements-}-}
 schools : MonsterSchools
 attack : TemporaryPermanentBase Attack
 defense : TemporaryPermanentBase Defense
 speed : TemporaryPermanentBase Speed
 range : TemporaryPermanentBase Range
 level : (Level,Level,Bounded 1 9)


 {-I might want to consider having separate soul cards with just soul skills.-}
 soulPoints : Nat {-Bounded 0 3-}
 aliveness : Aliveness

public export
record BasicSpell where
 constructor MkBasicSpell
 temporaryId : Nat
 {-schools : List Nat {-Vector of elements 0~5 of length 0, 1 or 2, with distinct elements-}-}
 school : School {-Spells must have exactly one school-}
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

