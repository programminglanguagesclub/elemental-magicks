import Data.Vect

data So : Bool -> Type where 
    Oh : So True
choose : (b : Bool) -> Either (So b) (So (not b))
choose True  = Left Oh
choose False = Right Oh


Bounded : Integer -> Integer -> Type
Bounded lower upper = (n ** So (n >= lower && n <= upper))


absoluteLowerBound : Integer
absoluteLowerBound = -1000

absoluteUpperBound : Integer
absoluteUpperBound = 1000

Range : Type
Range = Bounded 0 5

Speed : Type
Speed = Bounded absoluteLowerBound absoluteUpperBound

Defense : Type
Defense = Bounded 0 absoluteUpperBound

Attack : Type
Attack = Bounded 0 absoluteUpperBound

Level : Type
Level = Bounded 0 9 {- bounds for card level and schools -}
{- this should have a bound of 1 for base -}


CurrentModifiedBase : Type -> Type
CurrentModifiedBase t = (t,t,t)


transformBounded : (lower:Integer) -> (upper:Integer) -> So (lower >= lower && lower <= upper) -> So (upper >= lower && upper <= upper) -> (Integer -> Integer) -> Bounded lower upper -> Bounded lower upper
transformBounded lower upper ProofLower ProofUpper f (n ** _) =
 let m = f n in
  case (choose (m <= upper)) of
   Left ProofUpperBounded =>
    case (choose (m >= lower && (m <= upper))) of
     Left ProofBounded =>
      (m ** ProofBounded)
     Right _ =>
      (lower ** ProofLower) {- must make sure that lower is less than upper -}
   Right _ =>
    (upper ** ProofUpper)




transformRange : (Integer -> Integer) -> Range -> Range
transformRange = transformBounded 0 5 Oh Oh
transformSpeed : (Integer -> Integer) -> Speed -> Speed
transformSpeed = transformBounded absoluteLowerBound absoluteUpperBound Oh Oh
transformLevel : (Integer -> Integer) -> Level -> Level
transformLevel = transformBounded 0 9 Oh Oh
transformAttack : (Integer -> Integer) -> Attack -> Attack
transformAttack = transformBounded 0 absoluteUpperBound Oh Oh
{-for some reason it doesn't unify here if I replace the literals with the named constants-}






{- How do I do automatic, nonautomatic skill components? -}








syntax iff "(" [condition] ")" "{" [true_branch] "}" "else" "{" [false_branch] "}"
 = if condition then true_branch else false_branch

record Monster where
 constructor MkMonster
 attack : CurrentModifiedBase Attack
 defense : CurrentModifiedBase Defense
 speed : CurrentModifiedBase Speed
 range : CurrentModifiedBase Range
 level : CurrentModifiedBase Level

record Spell where
 constructor MkSpell
 level : Level


data Card = SpellCard Spell | MonsterCard Monster




{- I want this to be the records from before!!!! -}


{-
mutant_pig : Monster
mutant_pig = MkMonster ((20 ** Oh),(20 ** Oh),(20 ** Oh)) ((0 ** Oh),(0 ** Oh),(0 ** Oh)) ((2 ** Oh),(2 ** Oh),(2 ** Oh)) ((1 ** Oh),(1 ** Oh),(1 ** Oh)) ((3 ** Oh),(3 ** Oh),(3 ** Oh)) 
-}

syntax repeat3 [val] = ((val ** Oh),(val ** Oh),(val ** Oh))
syntax monster [attack] [defense] [speed] [range] [level] = MkMonster (repeat3 attack) (repeat3 defense) (repeat3 speed) (repeat3 range) (repeat3 level)
syntax spell [level] = MkSpell level


mutant_pig : Monster
mutant_pig = monster 20 0 2 1 3


foo : Card
foo = MonsterCard mutant_pig

{-syntax vectrepeat [val] [times] = if times == 0 then [] else val :: (vectrepeat [val] [times - 1])-}

Board : Type
Board = Vect 9 (Maybe Monster)
Hand : Type
Hand = List Card
Graveyard : Type
Graveyard = List Card
Spawn : Type
Spawn = Maybe Card
Soul : Type
Soul = Vect 5 (Maybe Monster) {- again more information could go in the type -}
Thoughts : Type
Thoughts = Bounded 0 absoluteUpperBound
Schools : Type
Schools = Vect 6 (Level)



{- A goes first in the first round; B goes first in the second round; -}




record Player where
 constructor MkPlayer
 board : Board
 hand : Hand
 graveyard : Graveyard
 spawn : Spawn
 soul : Soul
 thoughts : Thoughts
 schools : Schools
 token : String

syntax "new" "player" [token] = MkPlayer (Vect.replicate 9 Nothing) [] [] Nothing (Vect.replicate 5 Nothing) (0 ** Oh) (Vect.replicate 6 (0 ** Oh)) token



data Phase =
 DrawPhase
|SpawnPhase
|SpellPhase
|RemovalPhase
|StartPhase
|EngagementPhase
|EndPhase
|RevivalPhase

nextPhase : Phase -> Phase
nextPhase DrawPhase = SpawnPhase
nextPhase SpawnPhase = SpellPhase
nextPhase SpellPhase = RemovalPhase
nextPhase RemovalPhase = StartPhase
nextPhase StartPhase = EngagementPhase
nextPhase EngagementPhase = EndPhase
nextPhase EndPhase = RevivalPhase
nextPhase RevivalPhase = SpawnPhase



data AutomaticSkillComponent = DummyAutomaticSkillComponent {- I'll address this later... -}
data NonautomaticSkillComponent = DummyNonautomaticSkillComponent

SkillTail : Type
SkillTail = List NonautomaticSkillComponent

Skill : Type
Skill = (AutomaticSkillComponent, SkillTail)



{- still have to represent rounds, initiative -}

record Game where
 constructor MkGame
 round : Bounded 0 1
 skillHead : Maybe Skill
 skillQueue : List SkillTail

 player_A : Player
 player_B : Player

syntax "new" "game" [tokenA] [tokenB] = MkGame (0 ** Oh) Nothing [] (new player tokenA) (new player tokenB)


game : Game
game = new game "playerAToken" "playerBToken"


{-For now, I am ignoring spells.-}



data ClientUpdate = ClientDummy

data ServerUpdate = ServerDummy



ServerUpdateWrapper : Type
ServerUpdateWrapper = (ServerUpdate, String)



{-need to create update stuff, but for now ignore-}

{-
transformGame : Game -> ServerUpdate -> (Game, List ClientUpdate)
-}


while_loop : List Game -> ServerUpdate -> (List Game, List ClientUpdate)
while_loop [] _ = ([],[])
while_loop (g::gs) _ = ([],[])



main : IO ()
main = do {
 input <- getLine;
 putStrLn input;
}





