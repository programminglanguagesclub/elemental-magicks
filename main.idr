import Data.Vect
import Data.Fin


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



TemporaryPermanentBase : Type -> Type
TemporaryPermanentBase t = (t,t,t)


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



{-
ExtractBounded : Bounded lower upper -> Integer
ExtractBounded (n ** _) = n
-}



syntax iff "(" [condition] ")" "{" [true_branch] "}" "else" "{" [false_branch] "}"
 = if condition then true_branch else false_branch


data Aliveness = Alive | DeadFresh | DeadStale





record Monster where
 constructor MkMonster
 attack : TemporaryPermanentBase Attack
 defense : TemporaryPermanentBase Defense
 speed : TemporaryPermanentBase Speed
 range : TemporaryPermanentBase Range
 level : TemporaryPermanentBase Level
 aliveness : Aliveness

record Spell where
 constructor MkSpell
 level : Level


data Card = SpellCard Spell | MonsterCard Monster




syntax repeat3 [val] = ((val ** Oh),(val ** Oh),(val ** Oh))
syntax monster [attack] [defense] [speed] [range] [level] = MkMonster (repeat3 attack) (repeat3 defense) (repeat3 speed) (repeat3 range) (repeat3 level) Alive
syntax spell [level] = MkSpell level


mutant_pig : Monster
mutant_pig = monster 20 0 2 1 3


foo : Card
foo = MonsterCard mutant_pig

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
Knowledge : Type
Knowledge = Vect 6 (Level)



{- A goes first in the first round; B goes first in the second round; -}




record Player where
 constructor MkPlayer
 board : Board
 hand : Hand
 graveyard : Graveyard
 spawn : Spawn
 soul : Soul
 thoughts : Thoughts
 knowledge : Knowledge
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

{-
		SetHp:function(object){return ModifyCard(object)},
		SetMaxHp:function(object){return ModifyCard(object)},
-}



data Area = SpawnPosition | HandArea | GraveyardArea | DiscardArea
data Side = Friendly | Enemy



{- For now ignoring the client update part of this -}
{- I might be able to get away with doing a lot more of this with type classes -}



Set : Type
Set = (Side, Area)


{- not implementing universal quantifiers yet -}

data CardExistential = DeBruijnCardExistential Set

{- might have to index these entire datatypes with Fin... not sure... -}
data CardVar = BoundCardVar Card | UnBoundCardVar (Fin n) {- the idea is that once everything is bound, n will be 0, and we can just extract the card via matching -}

data BoardExistential = DeBruijnBoardExistential Side
data BoardVar = BoundBoardVar Monster | UnBoundBoardVar (Fin n) {- so essentially what these mean is "the n-1th variable that is not yet bound... or something like that" -}


BoardPredicate : Type
BoardPredicate = Maybe Monster -> Bool



MonsterExistsOnSquare : BoardPredicate
MonsterExistsOnSquare (Just m) = True
MonsterExistsOnSquare Nothing = False

MonsterAlive : BoardPredicate
MonsterAlive Nothing = False
MonsterAlive (Just m) with (aliveness m)
 | Alive = True
 | _ = False





CardPredicate : Type
CardPredicate = Card -> Bool





FieldIndex : Type
FieldIndex = Bounded 0 8

HandIndex : Type
HandIndex = Bounded 0 29

GraveyardIndex : Type
GraveyardIndex = Bounded 0 29

{- Hand and Graveyard indices point to the last available slot if they overshoot. The last available slot is the first one with no card in it. -}


data StatLValue = TemporaryL | PermanentL
data Mutator = IncrementL | SetL

School : Type
School = Bounded 0 5


data StatRValue = TemporaryR | PermanentR | BaseR


{- I need to put the constraints on the DeBruijn indices in the types here... -}
data LazyInt = Constant Integer
             | AttackR StatRValue CardVar
             | DefenseR StatRValue CardVar
             | RangeR StatRValue CardVar
             | SpeedR StatRValue CardVar
             | SchoolR School
             | ThoughtsR
             | SoulPointsR
             {- ignoring a few nice things like cardinality for now -}

data SkillEffect =
 Refresh Side FieldIndex {-if alive, restores all stats to base... could maybe not have this be a core thing. could even create syntactic sugar for this with syntax.... -}
 |IncrementHp FieldIndex LazyInt {- to be revisted when hp is defined -}
 |IncrementMaxHp FieldIndex LazyInt {- to be revisted when hp is defined -}
 |SendFromFieldToHand Side FieldIndex
 |SendFromFieldToGraveyard Side FieldIndex GraveyardIndex {-etc, but we can also abstract over FROM and TO...-}
 |SendFromGraveyardToGraveyard Side GraveyardIndex GraveyardIndex
 |SendFromGraveyardToHand Side GraveyardIndex HandIndex
 |SendFromHandToGraveyard Side HandIndex GraveyardIndex
 |SendFromHandToHand Side HandIndex HandIndex
 |SwitchSetAndHand Side HandIndex
 |SwitchFieldAndHand Side FieldIndex HandIndex
 |SwitchFieldAndGraveyard Side FieldIndex GraveyardIndex
 |SwitchFieldAndSet Side FieldIndex
 |SwitchHandAndSet Side HandIndex
 |SwitchGraveyardAndSet Side GraveyardIndex
 |SendFromHandToSet Side HandIndex{- no effect if set occupied or no monsters in hand -}
 |SendFromHandToField Side HandIndex FieldIndex {- no effect if field occupied at determined index or no monsters in hand -} 
 |SendFromGraveyardToField Side GraveyardIndex FieldIndex {- no effect if field occupied at determined index or no monsters in graveyard -}
{- there might be some more cases I'm forgetting right now.. -}
 |SetHp Side FieldIndex LazyInt {- to be revisted when hp is defined -}
 |SetMaxHp Side FieldIndex LazyInt {- to be revisted when hp is defined -}
 |LevelL Mutator StatLValue Side FieldIndex LazyInt
 |SpeedL Mutator StatLValue Side FieldIndex LazyInt
 |AttackL Mutator StatLValue Side FieldIndex LazyInt
 |DefenseL Mutator StatLValue Side FieldIndex LazyInt
 |RangeL Mutator StatLValue Side FieldIndex LazyInt
 |ReviveCard Side FieldIndex
 |EngagementL Side FieldIndex LazyInt
 |ThoughtsL Side Mutator LazyInt
 |KnowledgeL Mutator School Side LazyInt
 |TakeDamage Side FieldIndex LazyInt




{-Counter Skills should not trigger more than once per <SkillQueue is cleared completely> This way I don't have to worry about players running out of time from massively titanic runs of counterskill chains, or even nontermination-}



{- in order to allow for universals and existentials, I should not require FieldIndex here to be predetermined.... -}



FooBlargFoo : SkillEffect
FooBlargFoo = SpeedL SetL TemporaryL Friendly (2 ** Oh) (Constant 4)




BoardCondition : Type
BoardCondition = (List BoardExistential, BoardPredicate)

CardCondition : Type
CardCondition = (List CardExistential, CardPredicate)

data Condition = BoardCondition_ BoardCondition | CardCondition_ CardCondition

mutual
 NonautomaticSkillComponent : Type
 NonautomaticSkillComponent = (Condition,AutomaticSkillComponent)

 AutomaticSkillComponent : Type
 AutomaticSkillComponent = (List SkillEffect, Maybe NonautomaticSkillComponent)

SkillTailExample : List NonautomaticSkillComponent
SkillTailExample = []

Skill : Type
Skill = (AutomaticSkillComponent, List NonautomaticSkillComponent)

AutomaticSkillComponentExample : (List SkillEffect, Maybe NonautomaticSkillComponent)
AutomaticSkillComponentExample = ([], Nothing)



SkillExample : ((List SkillEffect, Maybe NonautomaticSkillComponent), List NonautomaticSkillComponent)
SkillExample = (AutomaticSkillComponentExample,SkillTailExample)






{- still have to represent rounds, initiative -}

record Game where
 constructor MkGame
 round : Bounded 0 1
 skillHead : Maybe Skill
 skillQueue : List (List NonautomaticSkillComponent)

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






data Ty = TyInt
        | TyBool
        | TyFun Ty Ty

interpTy : Ty -> Type
interpTy TyInt       = Int
interpTy TyBool      = Bool
interpTy (TyFun s t) = interpTy s -> interpTy t



{-index : Fin n -> Vect n a -> a
index FZ     (x::xs) = x
index (FS k) (x::xs) = index k xs
-}



using (G : Vect n Ty)
 data Env  : Vect n Ty -> Type where
      Nil  : Env Nil
      (::) : interpTy a -> Env G -> Env (a :: G)
 data HasType : (i : Fin n) -> Vect n Ty -> Ty -> Type where
  Stop : HasType FZ (t :: G) t
  Pop  : HasType k G t -> HasType (FS k) (u :: G) t














foo54 : Fin 4 {-0~3-}
foo54 = 3





























