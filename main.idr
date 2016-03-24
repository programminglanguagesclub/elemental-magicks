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


{-
Hp : Type
Hp = {n : Bounded absoluteLowerBound absoluteUpperBound} ->
     {m : Bounded absoluteLowerBound absoluteUpperBound} ->
     {x : So ((ExtractBounded n) < (ExtractBounded m))} ->
     ((n,m) ** So ((ExtractBounded n) <= (ExtractBounded m)))
-}





syntax iff "(" [condition] ")" "{" [true_branch] "}" "else" "{" [false_branch] "}"
 = if condition then true_branch else false_branch


data Aliveness = Alive | DeadFresh | DeadStale



{- Below I tried to make hp always <= max hp statically, but no success yet -}

{-
Eq Bounded absoluteLowerBound absoluteUpperBound where
 (==) n m = True
-}

{-
HH : Type
HH = Bounded absoluteLowerBound absoluteUpperBound

Eq HH where
 (==) n m = (==) (ExtractBounded n) (ExtractBounded m)
 


-}


{-show n = show (ExtractBounded n)-}




{-
Show (Bounded absoluteLowerBound absoluteUpperBound) where
 show (0 ** Oh) = "hello"
-}


{-
Show (Bounded absoluteLowerBound absoluteUpperBound) where
 show (_ ** _) = "hello"
-}



{-

What does it mean that type class implementation arguments have to be injective??

interface Foo a where
    bar : a -> Bool

Blarg : Type
Blarg = Bounded absoluteLowerBound absoluteUpperBound

Foo Blarg where
 bar m = True
-}




{-
Show Aliveness where
 show Alive = "Alive"
 show DeadFresh = "DeadFresh"
 show DeadStale = "DeadStale"
-}

Show Aliveness where
 show _ = "Alive"


Eq Aliveness where
 (==) Alive Alive = True
 (==) Alive DeadFresh = False
 (==) Alive DeadStale = False
 (==) DeadFresh Alive = False
 (==) DeadFresh DeadFresh = True
 (==) DeadFresh DeadStale = False
 (==) DeadStale Alive = False
 (==) DeadStale DeadFresh = False
 (==) DeadStale DeadStale = True




Ord Aliveness where
 compare Alive Alive = EQ
 compare Alive DeadFresh = GT
 compare Alive DeadStale = GT
 compare DeadFresh Alive = LT
 compare DeadFresh DeadFresh = EQ
 compare DeadFresh DeadStale = GT
 compare DeadStale Alive = LT
 compare DeadStale DeadFresh = LT
 compare DeadStale DeadStale = EQ

 (<) Alive Alive = True
 (<) Alive DeadFresh = False
 (<) Alive DeadStale = False
 (<) DeadFresh Alive = False
 (<) DeadFresh DeadFresh = True
 (<) DeadFresh DeadStale = False
 (<) DeadStale Alive = False
 (<) DeadStale DeadFresh = True
 (<) DeadStale DeadStale = True


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




{- I want this to be the records from before!!!! -}


{-
mutant_pig : Monster
mutant_pig = MkMonster ((20 ** Oh),(20 ** Oh),(20 ** Oh)) ((0 ** Oh),(0 ** Oh),(0 ** Oh)) ((2 ** Oh),(2 ** Oh),(2 ** Oh)) ((1 ** Oh),(1 ** Oh),(1 ** Oh)) ((3 ** Oh),(3 ** Oh),(3 ** Oh)) 
-}

syntax repeat3 [val] = ((val ** Oh),(val ** Oh),(val ** Oh))
syntax monster [attack] [defense] [speed] [range] [level] = MkMonster (repeat3 attack) (repeat3 defense) (repeat3 speed) (repeat3 range) (repeat3 level) Alive
syntax spell [level] = MkSpell level


{-  apparently instead of the syntax keyword, we can use type classes to do some of this. -}


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

IncrementPermanentAgility:function(object){return ModifyCard(object)},
		IncrementPermanentAttack:function(object){return ModifyCard(object)},
		IncrementPermanentDefense:function(object){return ModifyCard(object)},
		IncrementPermanentRange:function(object){return ModifyCard(object)},
		IncrementTemporaryAgility:function(object){return ModifyCard(object)},
		IncrementTemporaryAttack:function(object){return ModifyCard(object)},
		IncrementTemporaryDefense:function(object){return ModifyCard(object)},
		IncrementTemporaryRange:function(object){return ModifyCard(object)},
		IncrementTemporaryLevel:function(object){return ModifyCard(object)},
		IncrementEngagement:function(object){return ModifyCard(object)},
		IncrementThoughts:function(object){
			if(!validateTokens(object,["player","value","ignoredUniversals"])){
				console.error("invalid property in IncrementThoughts");
			}
			
			var retVal = {};
			if(object.player === undefined){
				retVal.player = {"truth":true,type:"True"};
			}
			else{
				retVal.player = Bool(object.player);
			}
			if(object.value === undefined){
				console.error("value not defined in IncrementThoughts");
			}
			retVal.value = Int(object.value);
			retVal.type = "IncrementThoughts";
			if(object.ignoredUniversals === undefined){
				retVal.ignoredUniversals = [];
			}
			else{
				retVal.ignoredUniversals = object.ignoredUniversals;
			}
			return retVal;
		},
		SendFromFieldToHand:function(object){
			if(!validateTokens(object,["player","fromIndex","toIndex","ignoredUniversals"])){
				console.error("invalid property in SendFromFieldToHand");
			}
			var retVal = {};
			if(object.player === undefined){
				retVal.player = {"truth":true,type:"True"};
			}
			else{
				retVal.player = Bool(object.player);
			}
			if(object.fromIndex === undefined){
				console.error("fromIndex not defined in SendFromFieldToHand");
			}
			retVal.fromIndex = Int(object.fromIndex);
			if(object.toIndex === undefined){
				console.error("toIndex not defined in SendFromFieldToHand");
			}
			retVal.toIndex = Int(object.toIndex);
			if(object.ignoredUniversals === undefined){
				retVal.ignoredUniversals = [];
			}
			else{
				retVal.ignoredUniversals = object.ignoredUniversals;
			}
			retVal.type = "SendFromFieldToHand";
			return retVal;
		},
		SendFromFieldToGraveyard:function(object){
			if(!validateTokens(object,["player","fromIndex","toIndex","ignoredUniversals"])){console.error("invalid property in SendFromFieldToGraveyard");}
			var retVal = {};
			if(object.player === undefined){retVal.player = {"truth":true,type:"True"};}
			else{retVal.player = Bool(object.player);}
			if(object.fromIndex === undefined){console.error("fromIndex not defined in SendFromFieldToGraveyard");}
			retVal.fromIndex = Int(object.fromIndex);
			if(object.toIndex === undefined){console.error("toIndex not defined in SendFromFieldToGraveyard");}
			retVal.toIndex = Int(object.toIndex);
			if(object.ignoredUniversals === undefined){
				retVal.ignoredUniversals = [];
			}
			else{
				retVal.ignoredUniversals = object.ignoredUniversals;
			}
			
			retVal.type = "SendFromFieldToGraveyard";
			return retVal;
		},
		SendFromGraveyardToGraveyard:function(object){
			if(!validateTokens(object,["player","fromIndex","toIndex","ignoredUniversals"])){console.error("invalid property in SendFromGraveyardToGraveyard");}
			var retVal = {};
			if(object.player === undefined){retVal.player = {"truth":true,type:"True"};}else{retVal.player = Bool(object.player);}
			if(object.fromIndex === undefined){console.error("fromIndex not defined in SendFromGraveyardToGraveyard");}
			retVal.fromIndex = Int(object.fromIndex);
			if(object.toIndex === undefined){console.error("toIndex not defined in SendFromGraveyardToGraveyard");}
			retVal.toIndex = Int(object.toIndex);
			if(object.ignoredUniversals === undefined){
				retVal.ignoredUniversals = [];
			}
			else{
				retVal.ignoredUniversals = object.ignoredUniversals;
			}
			retVal.type = "SendFromGraveyardToGraveyard";
			return retVal;
		},
		SendFromGraveyardToHand:function(object){
			if(!validateTokens(object,["player","fromIndex","toIndex","ignoredUniversals"])){console.error("invalid property in SendFromGraveyardToHand");}
			var retVal = {};
			if(object.player === undefined){retVal.player = {"truth":true,type:"True"};}else{retVal.player = Bool(object.player);}
			if(object.fromIndex === undefined){console.error("fromIndex not defined in SendFromGraveyardToHand");}
			retVal.fromIndex = Int(object.fromIndex);
			if(object.toIndex === undefined){console.error("toIndex not defined in SendFromGraveyardToHand");}
			retVal.toIndex = Int(object.toIndex);
			if(object.ignoredUniversals === undefined){
				retVal.ignoredUniversals = [];
			}
			else{
				retVal.ignoredUniversals = object.ignoredUniversals;
			}
			retVal.type = "SendFromGraveyardToHand";
			return retVal;
		},
		SendFromHandToGraveyard:function(object){
			if(!validateTokens(object,["player","fromIndex","toIndex","ignoredUniversals"])){console.error("invalid property in SendFromHandToGraveyard");}
			var retVal = {};
			if(object.player === undefined){retVal.player={"truth":true,type:"True"};}else{retVal.player = Bool(object.player);}
			if(object.fromIndex === undefined){console.error("fromIndex not defined in SendFromHandToGraveyard");}
			retVal.fromIndex = Int(object.fromIndex);
			if(object.toIndex === undefined){console.error("toIndex not defined in SendFromHandToGraveyard");}
			retVal.toIndex = Int(object.toIndex);
			if(object.ignoredUniversals === undefined){
				retVal.ignoredUniversals = [];
			}
			else{
				retVal.ignoredUniversals = object.ignoredUniversals;
			}
			retVal.type = "SendFromHandToGraveyard";
			return retVal;
		},
		SendFromHandToHand:function(object){
			if(!validateTokens(object,["player","fromIndex","toIndex","ignoredUniversals"])){console.error("invalid property in SendFromHandToHand");}
			var retVal = {};
			if(object.player === undefined){retVal.player = {"truth":true,type:"True"};}else{retVal.player = Bool(object.player);}
			if(object.fromIndex === undefined){console.error("fromIndex not defined in SendFromHandToHand");}
			retVal.fromIndex = Int(object.fromIndex);
			if(object.toIndex === undefined){console.error("toIndex not defined in SendFromHandToHand");}
			retVal.toIndex = Int(object.toIndex);
			if(object.ignoredUniversals === undefined){
				retVal.ignoredUniversals = [];
			}
			else{
				retVal.ignoredUniversals = object.ignoredUniversals;
			}
			retVal.type = "SendFromHandToHand";
			return retVal;
		},
		SwitchSetAndHand:function(object){
			if(!validateTokens(object,["player","handIndex","ignoredUniversals"])){console.error("invalid property in SwitchSetAndHand");}
			var retVal = {};
			if(object.player === undefined){retVal.player = {"truth":true,type:"True"};}else{retVal.player = Bool(object.player);}
			if(object.handIndex === undefined){console.error("handIndex not defined in SwitchSetAndHand");}
			retVal.handIndex = Int(object.handIndex);
			retVal.type = "SwitchSetAndHand";
			if(object.ignoredUniversals === undefined){
				retVal.ignoredUniversals = [];
			}
			else{
				retVal.ignoredUniversals = object.ignoredUniversals;
			}
			return retVal;
		},
		SwitchFieldAndHand:function(object){
			if(!validateTokens(object,["player","fieldIndex","handIndex","ignoredUniversals"])){console.error("invalid property in SwitchFieldAndHand");}
			var retVal = {};
			if(object.player === undefined){retVal.player = {"truth":true,type:"True"};}else{retVal.player = Bool(object.player);}
			if(object.fieldIndex === undefined){console.error("fieldIndex not defined in SwitchFieldAndHand");}
			retVal.fieldIndex = Int(object.fieldIndex);
			if(object.handIndex === undefined){console.error("handIndex not defined in SwitchFieldAndHand");}
			retVal.handIndex = Int(object.handIndex);
			retVal.type = "SwitchFieldAndHand";
			if(object.ignoredUniversals === undefined){
				retVal.ignoredUniversals = [];
			}
			else{
				retVal.ignoredUniversals = object.ignoredUniversals;
			}
			return retVal;
		},
		SwitchFieldAndGraveyard:function(object){
			if(!validateTokens(object,["player","fieldIndex","graveyardIndex","ignoredUniversals"])){console.error("invalid property in SwitchFieldAndGraveyard");}
			var retVal = {};
			if(object.player === undefined){retVal.player = {"truth":true,type:"True"};}else{retVal.player = Bool(object.player);}
			if(object.fieldIndex === undefined){console.error("fieldIndex not defined in SwitchFieldAndGraveyard");}
			retVal.fieldIndex = Int(object.fieldIndex);
			if(object.graveyardIndex === undefined){console.error("graveyardIndex not defined in SwitchFieldAndGraveyard");}
			retVal.graveyardIndex = Int(object.graveyardIndex);
			retVal.type = "SwitchFieldAndGraveyard";
			if(object.ignoredUniversals === undefined){
				retVal.ignoredUniversals = [];
			}
			else{
				retVal.ignoredUniversals = object.ignoredUniversals;
			}
			return retVal;
		},
		SetKnowledge:function(object){
			if(!validateTokens(object,["player","school","value","ignoredUniversals"])){console.error("invalid property in SetKnowledge");}
			var retVal = {};
			if(object.player === undefined){retVal.player={"truth":true,type:"True"};}else{retVal.player = Bool(object.player);}
			if(object.school === undefined){console.error("school not defined in SetKnowledge");}
			retVal.school = Int(object.school);
			if(object.value === undefined){console.error("value not defined in SetKnowledge");}
			retVal.value = Int(object.value);
			retVal.type = "SetKnowledge";
			if(object.ignoredUniversals === undefined){
				retVal.ignoredUniversals = [];
			}
			else{
				retVal.ignoredUniversals = object.ignoredUniversals;
			}
			return retVal;
		},
		SetHp:function(object){return ModifyCard(object)},
		SetMaxHp:function(object){return ModifyCard(object)},
		SetPermanentAgility:function(object){return ModifyCard(object)},
		SetPermanentAttack:function(object){return ModifyCard(object)},
		SetPermanentDefense:function(object){return ModifyCard(object)},
		SetPermanentRange:function(object){return ModifyCard(object)},
		SetPermanentLevel:function(object){return ModifyCard(object)},
		SetTemporaryAgility:function(object){return ModifyCard(object)},
		SetTemporaryAttack:function(object){return ModifyCard(object)},
		SetTemporaryDefense:function(object){return ModifyCard(object)},
		SetTemporaryRange:function(object){return ModifyCard(object)},
		SetTemporaryLevel:function(object){return ModifyCard(object)},
		ReviveCard:function(object){return ModifyCard(object)},
		SetEngagement:function(object){return ModifyCard(object)},
		SetThoughts:function(object){
			if(!validateTokens(object,["player","value","ignoredUniversals"])){console.error("invalid property in SetThoughts");}
			var retVal = {};
			if(object.player === undefined){retVal.player = {"truth":true,type:"True"};}else{retVal.player = Bool(object.player);}
			if(object.value === undefined){console.error("value not defined in IncrementThoughts");}
			retVal.value = Int(object.value);
			retVal.type = "SetThoughts";
			if(object.ignoredUniversals === undefined){
				retVal.ignoredUniversals = [];
			}
			else{
				retVal.ignoredUniversals = object.ignoredUniversals;
			}
			return retVal;
		},
		TakeDamage:function(object){return ModifyCard(object)}
	};
	return dispatch(object,constructables);






-}





data Area = SpawnPosition | HandArea | GraveyardArea | DiscardArea
data Side = Friendly | Enemy















{-

 I used to have both IF and WHILE statements in the original DSL.
 I'm removing WHILE statements because:

 1) Implementing WHILE is harder
 2) When implementing a skill that uses a WHILE statement, it is harder to make sure it terminates
 3) In order to ensure the game can progress at the expected temporal rate, we generally have an upper bound in mind for how long a WHILE statement can run anyway, and can potentially use several IF statements (not 100% sure if this will be clean)
 4) Not having both IF and WHILE statements means there is less pattern matching, and thus cleaner code.

-}



{- For now ignoring the client update part of this -}
{- I might be able to get away with doing a lot more of this with type classes -}



{- I can modularize the "permanent" part separately from the "Level" or whatever. Skipping for now. -}



Set : Type
Set = (Side, Area)

CardExistential : Type
CardExistential = (Set, String)


BoardExistential : Type
BoardExistential = (Side, String)



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

{-still have to put RValues in these. Like how much are you setting to / increasing by -}


data SkillEffect = {- I have to deal with the fact that these take varying amounts of arguments... -}
 Refresh Side FieldIndex Integer {-if alive, restores all stats to base... could maybe not have this be a core thing. could even create syntactic sugar for this with syntax.... -}
 |IncrementHp Integer
 |IncrementMaxHp Integer
 |SendFromFieldToHand Side FieldIndex
 |SendFromFieldToGraveyard Side FieldIndex GraveyardIndex {-etc, but we can also abstract over FROM and TO...-}
 |SendFromGraveyardToGraveyard Side GraveyardIndex GraveyardIndex
 |SendFromGraveyardToHand Side GraveyardIndex HandIndex
 |SendFromHandToGraveyard Side HandIndex GraveyardIndex
 |SendFromHandToHand Side HandIndex HandIndex
 |SwitchSetAndHand Side HandIndex
 |SwitchFieldAndHand Side FieldIndex HandIndex
 |SwitchFieldAndGraveyard Side FieldIndex GraveyardIndex
 |SetHp Side Integer
 |SetMaxHp Side Integer
 |LevelL Mutator StatLValue Side FieldIndex Integer
 |SpeedL Mutator StatLValue Side FieldIndex Integer
 |AttackL Mutator StatLValue Side FieldIndex Integer
 |DefenseL Mutator StatLValue Side FieldIndex Integer
 |RangeL Mutator StatLValue Side FieldIndex Integer
 |ReviveCard Side FieldIndex
 |EngagementL Side FieldIndex Integer
 |ThoughtsL Side Mutator Integer
 |KnowledgeL Mutator School Side Integer
 |TakeDamage Side FieldIndex Integer


data StatRValue = TemporaryR | PermanentR | BaseR

{-
data PossiblyBound = 
-}

{-
data LazyInt =
  AgilityR Existential Not Monster (But becomes monster after being bound or whatever)
 |AttackR Monster
 |DefenseR Monster
 |RangeR Monster
 |SpeedR Monster
 |KnowledgeR SchoolR?
 |ThoughtsR Side

-}

{-Counter Skills should not trigger more than once per <SkillQueue is cleared completely> This way I don't have to worry about players running out of time from massively titanic runs of counterskill chains, or even nontermination-}



{- in order to allow for universals and existentials, I should not require FieldIndex here to be predetermined.... -}



FooBlargFoo : SkillEffect
FooBlargFoo = SpeedL SetL TemporaryL Friendly (2 ** Oh) 4






















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





