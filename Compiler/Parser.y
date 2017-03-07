{
module Parser where    

import qualified Lexer

























import Text.Read

import Text.EditDistance

}


%name calc
%tokentype {Lexer.Token}
%error {parseError}
%monad {P} {thenP} {returnP}
%lexer {lexer} {(Lexer.Token Lexer.EOFToken (_) (_) (_))}


%token
 action {Lexer.Token Lexer.ActionSkill (_) (_) (_)}
 air {Lexer.Token Lexer.Air (_) (_) (_)}
 and {Lexer.Token Lexer.And (_) (_) (_)}
 assign {Lexer.Token Lexer.Assignment (_) (_) (_)}
 attack {Lexer.Token Lexer.Attack (_) (_) (_)}
 auto {Lexer.Token Lexer.AutoSkill (_) (_) (_)}
 banished {Lexer.Token Lexer.Banished (_) (_) (_)}
 base {Lexer.Token Lexer.Base (_) (_) (_)}
 colon {Lexer.Token Lexer.Colon (_) (_) (_)}
 comma {Lexer.Token Lexer.Comma (_) (_) (_)}
 condition {Lexer.Token Lexer.Condition (_) (_) (_)}
 contort {Lexer.Token Lexer.Contort (_) (_) (_)}
 cost {Lexer.Token Lexer.Cost (_) (_) (_)}
 counter {Lexer.Token Lexer.CounterSkill (_) (_) (_)}
 crush {Lexer.Token Lexer.Crush (_) (_) (_)}
 current {Lexer.Token Lexer.Temporary (_) (_) (_)}
 death {Lexer.Token Lexer.DeathSkill (_) (_) (_)}
 decrement {Lexer.Token Lexer.Decrement (_) (_) (_)}
 defense {Lexer.Token Lexer.Defense (_) (_) (_)}
 difference {Lexer.Token Lexer.Difference (_) (_) (_)}
 each {Lexer.Token Lexer.Each (_) (_) (_)}
 earth {Lexer.Token Lexer.Earth (_) (_) (_)}
 end {Lexer.Token Lexer.EndSkill (_) (_) (_)}
 enemy {Lexer.Token Lexer.Enemy (_) (_) (_)}
 engagement {Lexer.Token Lexer.Engagement (_) (_) (_)}
 eq {Lexer.Token Lexer.Equality (_) (_) (_)}
 field {Lexer.Token Lexer.Field (_) (_) (_)}
 fire {Lexer.Token Lexer.Fire (_) (_) (_)}
 for {Lexer.Token Lexer.For (_) (_) (_)}
 friendly {Lexer.Token Lexer.Friendly (_) (_) (_)}
 geq {Lexer.Token Lexer.GEQ (_) (_) (_)}
 graveyard {Lexer.Token Lexer.Graveyard (_) (_) (_)}
 gt {Lexer.Token Lexer.Gt (_) (_) (_)}
 hand {Lexer.Token Lexer.Hand (_) (_) (_)}
 hp {Lexer.Token Lexer.Hp (_) (_) (_)}
 if {Lexer.Token Lexer.If (_) (_) (_)}
 in {Lexer.Token Lexer.In (_) (_) (_)}
 increment {Lexer.Token Lexer.Increment (_) (_) (_)}
 lbracket {Lexer.Token Lexer.Lbracket (_) (_) (_)}
 leq {Lexer.Token Lexer.LEQ (_) (_) (_)}
 level {Lexer.Token Lexer.Level (_) (_) (_)}
 lparen  {Lexer.Token Lexer.Lparen (_) (_) (_)}
 lt {Lexer.Token Lexer.Lt (_) (_) (_)}
 max {Lexer.Token Lexer.Max (_) (_) (_)}
 mod {Lexer.Token Lexer.Mod (_) (_) (_)}
 name {Lexer.Token (Lexer.TargetString $$) (_) (_) (_)}
 not {Lexer.Token Lexer.Not (_) (_) (_)}
 number {Lexer.Token (Lexer.Number $$) (_) (_) (_) {-This is going to need a projection function probably??-}}
 or {Lexer.Token Lexer.Or (_) (_) (_)}
 permanent {Lexer.Token Lexer.Permanent (_) (_) (_)}
 product {Lexer.Token Lexer.Product (_) (_) (_)}
 quotient {Lexer.Token Lexer.Quotient (_) (_) (_)}
 range {Lexer.Token Lexer.Range (_) (_) (_)}
 rbracket {Lexer.Token Lexer.Rbracket (_) (_) (_)}
 rparen {Lexer.Token Lexer.Rparen (_) (_) (_)}
 select {Lexer.Token Lexer.Select (_) (_) (_)}
 self {Lexer.Token Lexer.Self (_) (_) (_)}
 semicolon {Lexer.Token Lexer.Semicolon (_) (_) (_)}
 soul {Lexer.Token Lexer.SoulSkill (_) (_) (_)}
 soulPoints {Lexer.Token Lexer.SoulPoints (_) (_) (_)}
 spawn {Lexer.Token Lexer.SpawnSkill (_) (_) (_)}
 speed {Lexer.Token Lexer.Speed (_) (_) (_)}
 spell {Lexer.Token Lexer.Spell (_) (_) (_)}
 spirit {Lexer.Token Lexer.Spirit (_) (_) (_)}
 start {Lexer.Token Lexer.StartSkill (_) (_) (_)}
 stretch {Lexer.Token Lexer.Stretch (_) (_) (_)}
 sum {Lexer.Token Lexer.Sum (_) (_) (_)}
 then {Lexer.Token Lexer.Then (_) (_) (_)}
 thought {Lexer.Token Lexer.Thought (_) (_) (_)}
 thoughts {Lexer.Token Lexer.Thoughts (_) (_) (_)}
 unable {Lexer.Token Lexer.Unable (_) (_) (_)}
 union {Lexer.Token Lexer.Union (_) (_) (_)}
 unit {Lexer.Token Lexer.Unit (_) (_) (_)}
 var {Lexer.Token (Lexer.Identifier $$) (_) (_) (_)}
 void {Lexer.Token Lexer.Void (_) (_) (_)}
 water {Lexer.Token Lexer.Water (_) (_) (_)}
 where {Lexer.Token Lexer.Where (_) (_) (_)}
 word {Lexer.Token (Lexer.Word $$) (_) (_) (_)}
 {-else {Lexer.Token Lexer.Else (_) (_) (_)}-}
 {-lex_error {Lexer.Token (Lexer.Error $$) (_) (_) (_)} {- WAIT A MINUTE... I DON'T WANT THIS!!! -}-}
 {-string {Lexer.Token (Lexer.TargetString $$) (_) (_) (_)}-}
{- eof {Lexer.Token Lexer.EOFToken (_) (_) (_)} {- NO NO NO I do not want this either!!!!! -}-}
   
   
   


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
School : word {Knowledge $1}
Schools : {Schools "NoSchools"}
        | word {Schools $1}
        | word word {Schools ($1 ++ $2)}
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
     | Expr gt Expr {Parser.GT $1 $3}
     | Expr geq Expr {GEQ $1 $3}
     | Expr lt Expr {Parser.LT $1 $3}
     | Expr leq Expr {LEQ $1 $3}
     | Expr eq Expr {Parser.EQ $1 $3}
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



{-
data TyKnowled = EarthKnowledge
               | FireKnowledge
               | WaterKnowledge
               | AirKnowledge
               | SpiritKnowledge
               | VoidKnowledge
               deriving Show


This can be after some typechecking...

-}


data Knowledge = Knowledge String
               deriving Show


data TypedSc = NoSchools
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


data Schools = Schools String
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


extractSurface :: Lexer.Token -> String
extractSurface (Lexer.Token _ _ _ s) = s

prettyPrint :: [String] -> String
prettyPrint [] = ""
prettyPrint (x:[]) = x
prettyPrint (x1:x2:xs) = x1 ++ " " ++ (prettyPrint (x2:xs))


}







