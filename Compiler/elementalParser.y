{
module Main where    

import qualified Lexer

}


%name calc
%tokentype {Lexer.Token}
%error {parseError}
%monad {P} {thenP} {returnP}
%lexer {lexer} {(Lexer.Token Lexer.EOFToken (_))}


%token
 then {Lexer.Token Lexer.Then (_)}
 for {Lexer.Token Lexer.For (_)}
 where {Lexer.Token Lexer.Where (_)}
 each {Lexer.Token Lexer.Each (_)}
 in {Lexer.Token Lexer.In (_)}
 unable {Lexer.Token Lexer.Unable (_)}
 unit {Lexer.Token Lexer.Unit (_)}
 spell {Lexer.Token Lexer.Spell (_)}
 assign {Lexer.Token Lexer.Assignment (_)}
 name {Lexer.Token (Lexer.TargetString $$) (_)}
 if {Lexer.Token Lexer.If (_)}
 {-else {Lexer.Token Lexer.Else (_)}-}
 var {Lexer.Token (Lexer.Identifier $$) (_)}
 colon {Lexer.Token Lexer.Colon (_)}
 lparen  {Lexer.Token Lexer.Lparen (_)}
 rparen {Lexer.Token Lexer.Rparen (_)}
 lbracket {Lexer.Token Lexer.Lbracket (_)}
 rbracket {Lexer.Token Lexer.Rbracket (_)}
 comma {Lexer.Token Lexer.Comma (_)}
 semicolon {Lexer.Token Lexer.Semicolon (_)}
 sum {Lexer.Token Lexer.Sum (_)}
 difference {Lexer.Token Lexer.Difference (_)}
 product {Lexer.Token Lexer.Product (_)}
 quotient {Lexer.Token Lexer.Quotient (_)}
 mod {Lexer.Token Lexer.Mod (_)}
 number {Lexer.Token (Lexer.Number $$) (_) {-This is going to need a projection function probably??-}}
 {-string {Lexer.Token (Lexer.TargetString $$) (_)}-}
 {-lex_error {Lexer.Token (Lexer.Error $$) (_)} {- WAIT A MINUTE... I DON'T WANT THIS!!! -}-}
 eq {Lexer.Token Lexer.Equality (_)}
 leq {Lexer.Token Lexer.LEQ (_)}
 lt {Lexer.Token Lexer.Lt (_)}
 geq {Lexer.Token Lexer.GEQ (_)}
 gt {Lexer.Token Lexer.Gt (_)}
 and {Lexer.Token Lexer.And (_)}
 or {Lexer.Token Lexer.Or (_)}
 not {Lexer.Token Lexer.Not (_)}
{- eof {Lexer.Token Lexer.EOFToken (_)} {- NO NO NO I do not want this either!!!!! -}-}
 level {Lexer.Token Lexer.Level (_)}
 attack {Lexer.Token Lexer.Attack (_)}
 defense {Lexer.Token Lexer.Defense (_)}
 speed {Lexer.Token Lexer.Speed (_)}
 range {Lexer.Token Lexer.Range (_)}
 soulPoints {Lexer.Token Lexer.SoulPoints (_)}
 earth {Lexer.Token Lexer.Earth (_)}
 fire {Lexer.Token Lexer.Fire (_)}
 water {Lexer.Token Lexer.Water (_)}
 air {Lexer.Token Lexer.Air (_)}
 spirit {Lexer.Token Lexer.Spirit (_)}
 void {Lexer.Token Lexer.Void (_)}
 start {Lexer.Token Lexer.StartSkill (_)}
 end {Lexer.Token Lexer.EndSkill (_)}
 counter {Lexer.Token Lexer.CounterSkill (_)}
 spawn {Lexer.Token Lexer.SpawnSkill (_)}
 death {Lexer.Token Lexer.DeathSkill (_)}
 auto {Lexer.Token Lexer.AutoSkill (_)}
 action {Lexer.Token Lexer.ActionSkill (_)}
 soul {Lexer.Token Lexer.SoulSkill (_)}
 cost {Lexer.Token Lexer.Cost (_)}
 condition {Lexer.Token Lexer.Condition (_)}
 self {Lexer.Token Lexer.Self (_)}
 thoughts {Lexer.Token Lexer.Thoughts (_)}
 thought {Lexer.Token Lexer.Thought (_)}
 max {Lexer.Token Lexer.Max (_)}
 hp {Lexer.Token Lexer.Hp (_)}
 base {Lexer.Token Lexer.Base (_)}
 permanent {Lexer.Token Lexer.Permanent (_)}
 current {Lexer.Token Lexer.Temporary (_)}
 engagement {Lexer.Token Lexer.Engagement (_)}
 friendly {Lexer.Token Lexer.Friendly (_)}
 enemy {Lexer.Token Lexer.Enemy (_)}
 increment {Lexer.Token Lexer.Increment (_)}
 decrement {Lexer.Token Lexer.Decrement (_)}
 stretch {Lexer.Token Lexer.Stretch (_)}
 crush {Lexer.Token Lexer.Crush (_)}
 contort {Lexer.Token Lexer.Contort (_)}
 select {Lexer.Token Lexer.Select (_)}
 hand {Lexer.Token Lexer.Hand (_)}
 field {Lexer.Token Lexer.Field (_)}
 graveyard {Lexer.Token Lexer.Graveyard (_)}
 banished {Lexer.Token Lexer.Banished (_)}
 union {Lexer.Token Lexer.Union (_)}
   
   
   


%nonassoc gt lt eq leq geq

%left equals


%left sum difference mod
%left product quotient
%left union

%right not
%left or
%left and



%left dot



%%

File : Units Spells {File $1 $2}
Units : {[]}
      | Unit Units {$1 : $2}
Spells : {[]}
       | Spell Spells {$1 : $2}
Unit : unit name Stats Start End Counter Spawn Death Auto Actions Soul {Unit $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 {-Should make sure in the type checker that the list of LValues is nonempty-}}
Spell : spell name School level colon number spawn colon Skill {Spell $2 $3 $6 $9}
Stats : Schools level colon number hp colon number attack colon number defense colon number speed colon number range colon number soulPoints colon number {Stats $1 $4 $7 $10 $13 $16 $19 $22}
School : earth {EarthKnowledge}
       | fire {FireKnowledge}
       | water {WaterKnowledge}
       | air {AirKnowledge}
       | spirit {SpiritKnowledge}
       | void {VoidKnowledge}
Schools : {NoSchools}
        | earth {Earth}
        | fire {Fire}
        | water {Water}
        | air {Air}
        | spirit {Spirit}
        | void {Void}
        | earth fire {EarthFire}
        | earth water {EarthWater}
        | earth air {EarthAir}
        | earth spirit {EarthSpirit}
        | earth void {EarthVoid}
        | fire water {FireWater}
        | fire air {FireAir}
        | fire spirit {FireSpirit}
        | fire void {FireVoid}
        | water air {WaterAir}
        | water spirit {WaterSpirit}
        | water void {WaterVoid}
        | air spirit {AirSpirit}
        | air void {AirVoid}
        | spirit void {SpiritVoid}
Start : {Nothing}
      | start colon Skill {Just $ Start $3}
End : {Nothing}
    | end colon Skill {Just $ End $3}
Counter : {Nothing}
        | counter colon Skill {Just $ Counter $3}
Spawn : {Nothing}
      | spawn colon Skill {Just $ Spawn $3}
Death : {Nothing}
      | death colon Skill {Just $ Death $3}
Auto : {Nothing}
     | auto colon Skill {Just $ Auto $3}
Actions : {[]}
        | Action Actions {$1 : $2}
Action : action colon Skill {Action $3}
Soul : soul colon Skill {Soul $3}
Skill : OptionalCost OptionalCondition Automatic {AutomaticSkill $1 $2 $3}
OptionalCost : {Constant "0"}
             | cost colon number {Constant $3}
OptionalCondition : {Always}
                  | condition colon Expr {$3}
OptionalFilter : {Always}
               | where Expr {$2}
Nonautomatic : {TerminatedSkillComponent}
             | select SelectionStatement NullableExpr ThenCase IfUnableCase NextAutomatic {Nonautomatic $2 $3 $4 $5 $6}
Automatic : SkillEffects Nonautomatic {Automatic $1 $2 {-Ignoring Universal case for now-} }
          | for each var in Side RelativeSet OptionalFilter comma {undefined {-Only allow one universally quantified variable at once. No pairs -}}
ThenCase : then lbracket Automatic rbracket {$3}
IfUnableCase : {Automatic [] TerminatedSkillComponent}
             | if unable lbracket Automatic rbracket {$4}
NextAutomatic : {Automatic [] TerminatedSkillComponent}
              | lbracket Automatic rbracket {$2}
SelectionStatement : Variables in Set RestSelectionStatement {[] ++ $4}
Set : Side RelativeSet {SimpleSet $1 $2}
    | Set union Set {UnionSet $1 $3}
RestSelectionStatement : {[]}
                       | comma Variables in Set RestSelectionStatement { (getFoo $2 $4) ++ $5}
Variables : var RestVariable {$1 : $2}
RestVariable : {[]}
             | comma var RestVariable {$2 : $3}
SkillEffects : {[]}
             | SkillEffect semicolon SkillEffects {$1 : $3}

SkillEffect : Assignment {SkillEffectAssignment $1}
Assignment : lparen ListExpr rparen Mutator Expr {Assignment $2 $4 $5}
Mutator : assign {Set}
        | increment {Increment}
        | decrement {Decrement}
        | stretch {Stretch}
        | crush {Crush}
        | contort {Contort}
ListExpr : {[]}
         | Expr {[$1]}
         | Expr comma ListExprCommas {$1 : $3}
ListExprCommas : Expr {[$1]}
               | Expr comma ListExprCommas {$1 : $3}
NullableExpr : {Always}
             | Expr {$1}
Expr : number {Constant $1}
     | Field self {Self $1}
     | Field var {Var $1 $2}
     | Side School {KnowledgeExpr $2 $1}
     | Side thoughts {ThoughtsExpr $1 {-CURRENTLY DO NOT HAVE ERROR MESSAGE IF PLURALITY WRONG-}}
     | Side thought {ThoughtsExpr $1 }
     | Expr sum Expr {Sum $1 $3}
     | Expr difference Expr {Difference $1 $3}
     | Expr product Expr {Product $1 $3}
     | Expr quotient Expr {Quotient $1 $3}
     | Expr mod Expr {Mod $1 $3} 
     | Expr gt Expr {Main.GT $1 $3}
     | Expr geq Expr {GEQ $1 $3}
     | Expr lt Expr {Main.LT $1 $3}
     | Expr leq Expr {LEQ $1 $3}
     | Expr eq Expr {Main.EQ $1 $3}
     | Expr and Expr {And $1 $3}
     | Expr or Expr {Or $1 $3}
     | not Expr {Not $2}
Field : Stat Temporality {StatField $1 $2}
      | HpStat {HpStatField $1}
      | engagement {EngagementField}  
Temporality : current {Temporary}
            | permanent {Permanent}
            | base {Base}
HpStat : hp {CurrentHp}
       | max hp {MaxHp}
       | base hp {BaseHp}
Stat : attack {Attack}
     | defense {Defense}
     | speed {Speed}
     | range {Range}
     | level {Level}
Side : friendly {Friendly}
     | enemy {Enemy}
RelativeSet : field {Field}
            | hand {Hand}
            | graveyard {Graveyard}
            | banished {Banished}
            | spawn {SpawnLocation}


{



getFoo :: [String] -> Set -> [(String,Set)]
getFoo _ _ = undefined 







type P a = Lexer.Alex a



thenP = (>>=)

returnP = return
{-
failP = fail
-}
catchP m c = fail "catch not implemented"











{-
fooBar :: ((Token,Int) -> P a) -> P a
       fooBar  = (alexMonadScan >>=)
       -}



lexer :: (Lexer.Token -> P a) -> P a
lexer = (Lexer.alexMonadScan >>=)






{-

At this point an important design decision has to be made regarding statements of the form

base hp x := <RValue>



In particular, it is probably easiest to accept these at the level of the parser,
and then reject them later at the type-checking phase, so that error messages will benefit from parsing.


That is, in this case, we will know that we have an attempted assignment, so we can output a message of the form

"You are not allowed to assign to base stats"



If we rejected this with a parse error, we would have much more difficulty displaying useful error messages.


-}






{-I'm currently throwing away typing information in my AST.-}

{-
parseError :: [Lexer.Token] -> a
parseError tokens = error $ ("ERROR: " ++ show tokens) {-error "Parse error"-}
-}

parseError tokens = do
 i <- Lexer.getLineNumber
 Lexer.alexError $ show i



data File = File [Unit] [Spell]
             deriving Show

data Unit = Unit String Stats (Maybe Start) (Maybe End) (Maybe Counter) (Maybe Spawn) (Maybe Death) (Maybe Auto) [Action] Soul
            deriving Show
data Spell = Spell String Knowledge String Skill {-name, school, level, skill-}
             deriving Show

data Skill = AutomaticSkill Expr Expr Automatic
           | NonautomaticSkill Expr Expr Nonautomatic 
           deriving Show
{-Cost, Condition, skill-}



{-

This is one place where syntatic sugar becomes interesting.

I am creating a data structure which captures the surface level syntax, even though this is a terrible of representing the data for other purposes.

This allows for better errors messages to be printed.


-}


data VariableBindings = VariableBindings [([String], Set)]


data Side = Friendly
          | Enemy
          deriving Show
            

data RelativeSet = Field
                 | Hand
                 | Graveyard
                 | Banished
                 | SpawnLocation
                 deriving Show


data Set = SimpleSet Side RelativeSet
         | UnionSet Set Set
         deriving Show



data SkillEffect = SkillEffectAssignment Assignment
                 deriving Show
data Nonautomatic = Nonautomatic [(String, Set)] Expr Automatic Automatic Automatic {-variables, where condition-} | TerminatedSkillComponent
                  deriving Show
data Automatic = Automatic [SkillEffect] Nonautomatic
               deriving Show

data Stats = Stats Schools String String String String String String String
           deriving Show
data Start = Start Skill
           deriving Show
data End = End Skill
           deriving Show
data Counter = Counter Skill
            deriving Show
data Spawn = Spawn Skill
            deriving Show
data Death = Death Skill
            deriving Show
data Auto = Auto Skill
           deriving Show
data Action = Action Skill
            deriving Show
data Soul = Soul Skill
           deriving Show

data Stat = Attack | Defense | Speed | Range | Level 
          deriving Show
data Mutator = Increment | Decrement | Stretch | Crush | Contort | Set
             deriving Show
data Temporality = Temporary | Permanent | Base
                 deriving Show
data HpStat = CurrentHp | MaxHp | BaseHp
            deriving Show
data Engagement = Engagement
                deriving Show


data Knowledge = EarthKnowledge
               | FireKnowledge
               | WaterKnowledge
               | AirKnowledge
               | SpiritKnowledge
               | VoidKnowledge
               deriving Show
data Schools = NoSchools
             | Earth
             | Fire
             | Water
             | Air
             | Spirit
             | Void
             | EarthFire
             | EarthWater
             | EarthAir
             | EarthSpirit
             | EarthVoid
             | FireWater
             | FireAir
             | FireSpirit
             | FireVoid
             | WaterAir
             | WaterSpirit
             | WaterVoid
             | AirSpirit
             | AirVoid
             | SpiritVoid
             deriving Show






{- do not forget engagement -}


data Assignment = Assignment [Expr] Mutator Expr
                deriving Show
data Field = StatField Stat Temporality
           | HpStatField HpStat
           | EngagementField
           deriving Show

{-Effects that happen to two units simultaneously trigger resulting effects by order of field position, with
ties broken by initiative. That is the significance of assigning two values at the "same" time-}


data Expr = Constant String
          | ThoughtsExpr Side
          | KnowledgeExpr Knowledge Side
          | Self Field
          | Var Field String
          | Sum Expr Expr
          | Difference Expr Expr
          | Product Expr Expr
          | Quotient Expr Expr
          | Mod Expr Expr
          | Always {-add more booleans later......-}
          | GT Expr Expr
          | GEQ Expr Expr
          | LT Expr Expr
          | LEQ Expr Expr
          | EQ Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
           deriving Show


getTokens :: String -> [Lexer.Token] {-For now, no error handling-}
getTokens s = case Lexer.runAlex s Lexer.gather of Left _ -> []
                                                   Right x -> x {-(map fst x)-}


{-
getTokens :: String -> [Lexer.Token] {-For now, no error handling-}
getTokens s = case Lexer.runAlex s Lexer.gather of
                   Left _ -> []
                   Right x -> (map fst x)
-}

{-
programPrint :: File -> IO ()
programPrint p = print p
-}

{-
getProgram :: IO File
getProgram = do
             s <- getContents
             pure (calc $ getTokens s)
-}

{-
main = pure (){-do
       x <- getProgram
       programPrint x
       -}
-}


main = do
 x <- getContents
 case Lexer.runAlex x calc of
  Right y -> error $ show y
  Left y -> error $ show {-x-} y

}






