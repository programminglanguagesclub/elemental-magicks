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










{- rename this BoardIndex -}

FieldIndex : Type
FieldIndex = Bounded 0 8

















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







Env : Nat -> Nat -> Nat -> Type
Env m n p = (Vect m Monster, Vect n FieldIndex, Vect p Card)


{- For now, I am not putting the requirement that entries be unique into the type. This is IMPORTANT, but it is unclear to me now where it should be handled (could even be done in Ur/Web) -}
{-also for now cannot target discard.-}

{-
Selection : (b : Nat) -> (h : Nat) -> (g : Nat) -> (Vect b FieldIndex, Vect h (Bounded howevermany there are in the hand at the moment.....)
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



{-not sure if I want Fin n or Nat here...-}

data CardVar : Nat -> Type where
 BoundCardVar : Card -> CardVar 0
 UnBoundCardVar : (Fin n) -> (CardVar n)






{- The trick here is that for the purpose of selecting squares on the board I don't really want "Maybe Monster". If they select a monster,
I want that to be the same monster even if it gets moved on the board. In the case of an empty square, however, it has to be that particular coordinate.

It's worth nothing though that this means that in the case of empty squares that are no longer empty, we have a problem.

Equally though, we have an issue with monsters that have moved to a region that does not allow certain skills to be applied with them in their former capacity.


For now, it's acceptable to represent the square number and monster at the type level, and have a value level check to see if it's still valid.

Then if it's not valid, the skill can terminate (or at least that part of it).

This may be the best solution in the end because we may wish to allow skills to have the possibility of doing this depending on
game state or player action.




-}




{- Might need another type for the Spawn position?? -}
{- Rename set to spawn everywhere -}



{- the idea is that once everything is bound, n will be 0, and we can just extract the card via matching -}



data BoardMonsterExistential = DeBruijnBoardMonsterExistential Side
data BoardMonsterVar = BoundBoardMonsterVar Monster | UnBoundBoardMonsterVar (Fin n) {- so essentially what these mean is "the n-1th variable that is not yet bound... or something like that" -}


BoardMonsterPredicate : Type
BoardMonsterPredicate = Monster -> Bool


data BoardSquareExistential = DeBruijnBoardSquareExistential Side
data BoardSquareVar = BoundBoardSquareVar FieldIndex | UnBoundBoardSquareVar (Fin n)


{-I might need more information... I Might need to pass in the game object into these really...
-}
BoardSquarePredicate : Type
BoardSquarePredicate = FieldIndex -> Bool




MonsterAlive : BoardMonsterPredicate
MonsterAlive m with (aliveness m)
 | Alive = True
 | _ = False





CardPredicate : Type
CardPredicate = Card -> Bool





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
data LazyInt : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type where
 {-Constant : Integer -> LazyInt 0 -} {-ignoring constant for now. It's neither bound to a card nor awaiting binding, so it's an edge case for this design-}
 BoardAttackR : {m : Nat} -> {n : Nat} -> {p : Nat} -> {env : Env m n p} -> StatRValue -> Fin n -> LazyInt m n p env



{-
             | AttackR StatRValue CardVar
             | DefenseR StatRValue CardVar
             | RangeR StatRValue CardVar
             | SpeedR StatRValue CardVar
             | SchoolR School
             | ThoughtsR
             | SoulPointsR
             {- ignoring a few nice things like cardinality for now -}

-}




{- Need to keep track of the accessing index so Fin n Fin m Fin p goes somewhere around here... in LazyInt somewhere? How do I fit it? -}

data SkillEffect : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type where
 AttackL : {m : Nat} -> {n : Nat} -> {p : Nat} -> {env : Env m n p} -> Mutator -> StatLValue -> Side -> FieldIndex -> (LazyInt m n p env) -> SkillEffect m n p env




{- can I write {n m p : Nat} -> ??? -}

{-
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

-}


{-Counter Skills should not trigger more than once per <SkillQueue is cleared completely> This way I don't have to worry about players running out of time from massively titanic runs of counterskill chains, or even nontermination-}



{- in order to allow for universals and existentials, I should not require FieldIndex here to be predetermined.... -}


{-
FooBlargFoo : SkillEffect
FooBlargFoo = SpeedL SetL TemporaryL Friendly (2 ** Oh) (Constant 4)
-}



BoardSquareCondition : Nat -> Type
BoardSquareCondition n = (Vect n BoardSquareExistential, BoardSquarePredicate)

BoardMonsterCondition : Nat -> Type
BoardMonsterCondition n = (Vect n BoardMonsterExistential, BoardMonsterPredicate)

CardCondition : Nat -> Type
CardCondition n = (Vect n CardExistential, CardPredicate)
















{- Again, actually need 2-3 Nats. -}


{-Nats refer to: Board, and then Graveyard | Hand | Discard, and then Spawn position.-}



{- Need a way to combine this with earlier conditions so that the predicate can reference earlier existentials... -}

{- for now ignoring whatever a set condition would be. It's not an existential. Maybe a universal...-}

data Condition : Nat -> Nat -> Nat -> Type where
 BoardMonsterCondition_ : (BoardMonsterCondition n) -> Condition n 0 0
 BoardSquareCondition_ : (BoardSquareCondition n) -> Condition n 0 0
 CardCondition_ : (CardCondition n) -> Condition 0 n 0







{- Somewhere I have to define Env, which is going to be a Vect of bound (whatever)s -}









{- these need to be made into datatypes because they need to store Env.-}
mutual






{- Need to concat in the env here!!!!-}
{-

Ah, so actually statement true is currently
AutomaticSkillComponent (m) (n) (p) env,
and it gets mapped to
AutomaticSkillComponent (m+m') (n+n') (p+p') env'

only when we are actually able to update env (produce env' that is),
which happens in the small-step interpreter (when we get the bindings from Condition, in other words)

-}
 NonautomaticSkillComponent : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type
 NonautomaticSkillComponent m n p env = {m' : Nat} -> {n' : Nat} -> {p' : Nat} -> ((Condition m' n' p'), AutomaticSkillComponent m n p env, AutomaticSkillComponent m n p env, AutomaticSkillComponent m n p env)
{- in particular n could be different for this last one-}


{- this is if(condition){do if true}{do if false}{do after if statement}-}
{- not n in all cases here. Again, just trying to get to typecheck for now. -}
{- Also there need to be two natural indices: one for selection of cards, and the other for selection of squares! -}
{- Oh, then there's everything regarding the set location. That's a bit tricky because you shouldn't be able to affect its stats, so it's like a Card, but it can be empty, which is like a Square...-}
{- Actually, the set location doesn't require being selected, although it does get bound (you can do things to the card that was on the set position at a particular time) -}
{- maybe it doesn't get bound -}

{-
I have two mutually incompatible ways that "next" is being implemented.
One is I have it built directly into NonautomaticSkillComponent (instead of just having StatementTrue and StatementFalse).
The other is that in AutomaticSkillComponent I have a List of NonautomaticSkillComponents (instead of just Maybe NonautomaticSkillComponent)
-}



{- These skill effects are bounded by m n and p, but I want it to type check if it has less restricted bounds... -}


{-
 TTT : (n : Nat) -> Type
-}

 AutomaticSkillComponent : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type
 AutomaticSkillComponent m n p env = (List (SkillEffect m n p env), Maybe (NonautomaticSkillComponent m n p env))

{-
SkillTailExample : List (NonautomaticSkillComponent 4 4 4)
SkillTailExample = []
-}

{-
Skill : Nat -> Nat -> Nat -> Type
Skill m n p = (AutomaticSkillComponent m n p, List (NonautomaticSkillComponent m n p))
-}
{-
Skill : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type
Skill m n p env = (AutomaticSkillComponent m n p env, List (NonautomaticSkillComponent m n p env))
-}


Skill : (m : Nat) -> (n : Nat) -> (p : Nat) -> (Env m n p) -> Type
Skill m n p env = AutomaticSkillComponent m n p env







{- I should wrap nonautomatic skill componenets in a datatype so that I don't have ot have
the same m n p for all of them...

Or wait... m n and p for them is always 0 in the case of the skill queue...

-}

{- still have to represent rounds, initiative -}

record Game where
 constructor MkGame
 round : Bounded 0 1
 m : Nat
 n : Nat
 p : Nat

{- skillHead index. I might want skillHead to not be a maybe, and consider "Nothing" to be where n = 0. Not sure. -}
 
 env : Env m n p
 empty_env : Env 0 0 0
 skillHead : Maybe (Skill m n p env)
 skillQueue : List (NonautomaticSkillComponent 0 0 0 empty_env)

 player_A : Player
 player_B : Player



syntax "new" "game" [tokenA] [tokenB] = MkGame (0 ** Oh) 0 0 0 (Vect.Nil,Vect.Nil,Vect.Nil) (Vect.Nil,Vect.Nil,Vect.Nil) Nothing [] (new player tokenA) (new player tokenB)


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


{-

while_loop : List Game -> ServerUpdate -> (List Game, List ClientUpdate)
while_loop [] _ = ([],[])
while_loop (g::gs) _ = ([],[])

-}






















































main : IO ()
main =

 putStrLn (show (Vect.nub [3,3,2,3,1,2,1,1,5,4,3,534,2,1]))

{- do {
 input <- getLine;
 putStrLn input;
}
-}





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


{-
I used n above and now it's confused with this...

using (G : Vect n Ty)
 data Env  : Vect n Ty -> Type where
      Nil  : Env Nil
      (::) : interpTy a -> Env G -> Env (a :: G)
 data HasType : (i : Fin n) -> Vect n Ty -> Ty -> Type where
  Stop : HasType FZ (t :: G) t
  Pop  : HasType k G t -> HasType (FS k) (u :: G) t

-}












foo54 : Fin 4 {-0~3-}
foo54 = 3
















