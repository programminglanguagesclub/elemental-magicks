{
module Parser where    
import qualified Lexer


import qualified Data.Map.Strict as HashMap



















import Text.Read

import Text.EditDistance

}


%name calc
%tokentype {Lexer.Token}
%error {parseError}
%monad {P} {thenP} {returnP}
%lexer {lexer} {(Lexer.Token Lexer.EOFToken (_))}


%token
 action {Lexer.Token Lexer.ActionSkill (_)}
 and {Lexer.Token Lexer.And (_)}
 assign {Lexer.Token Lexer.Assignment (_)}
 attack {Lexer.Token Lexer.Attack $$}
 auto {Lexer.Token Lexer.AutoSkill (_)}
 banished {Lexer.Token Lexer.Banished (_)}
 base {Lexer.Token Lexer.Base (_)}
 colon {Lexer.Token Lexer.Colon (_)}
 comma {Lexer.Token Lexer.Comma (_)}
 condition {Lexer.Token Lexer.Condition (_)}
 contort {Lexer.Token Lexer.Contort (_)}
 cost {Lexer.Token Lexer.Cost (_)}
 counter {Lexer.Token Lexer.CounterSkill (_)}
 crush {Lexer.Token Lexer.Crush (_)}
 current {Lexer.Token Lexer.Temporary (_)}
 death {Lexer.Token Lexer.DeathSkill (_)}
 decrement {Lexer.Token Lexer.Decrement (_)}
 defense {Lexer.Token Lexer.Defense $$}
 difference {Lexer.Token Lexer.Difference (_)}
 each {Lexer.Token Lexer.Each (_)}
 end {Lexer.Token Lexer.EndSkill (_)}
 enemy {Lexer.Token Lexer.Enemy (_)}
 engagement {Lexer.Token Lexer.Engagement (_)}
 eq {Lexer.Token Lexer.Equality (_)}
 field {Lexer.Token Lexer.Field (_)}
 for {Lexer.Token Lexer.For (_)}
 friendly {Lexer.Token Lexer.Friendly (_)}
 geq {Lexer.Token Lexer.GEQ (_)}
 graveyard {Lexer.Token Lexer.Graveyard (_)}
 gt {Lexer.Token Lexer.Gt (_)}
 hand {Lexer.Token Lexer.Hand (_)}
 hp {Lexer.Token Lexer.Hp $$}
 if {Lexer.Token Lexer.If (_)}
 in {Lexer.Token Lexer.In (_)}
 increment {Lexer.Token Lexer.Increment (_)}
 lbracket {Lexer.Token Lexer.Lbracket (_)}
 leq {Lexer.Token Lexer.LEQ (_)}
 level {Lexer.Token Lexer.Level (_)}
 lparen  {Lexer.Token Lexer.Lparen (_)}
 lt {Lexer.Token Lexer.Lt (_)}
 max {Lexer.Token Lexer.Max (_)}
 mod {Lexer.Token Lexer.Mod (_)}
 name {Lexer.Token (Lexer.TargetString $$) (_)}
 not {Lexer.Token Lexer.Not (_)}
 number {Lexer.Token (Lexer.Number _) $$  {-This is going to need a projection function probably??-}}
 or {Lexer.Token Lexer.Or (_)}
 permanent {Lexer.Token Lexer.Permanent (_)}
 product {Lexer.Token Lexer.Product (_)}
 quotient {Lexer.Token Lexer.Quotient (_)}
 range {Lexer.Token Lexer.Range $$}
 rbracket {Lexer.Token Lexer.Rbracket (_)}
 rparen {Lexer.Token Lexer.Rparen (_)}
 select {Lexer.Token Lexer.Select (_)}
 self {Lexer.Token Lexer.Self (_)}
 semicolon {Lexer.Token Lexer.Semicolon (_)}
 soul {Lexer.Token Lexer.SoulSkill (_)}
 soulPoints {Lexer.Token Lexer.SoulPoints (_)}
 spawn {Lexer.Token Lexer.SpawnSkill (_)}
 speed {Lexer.Token Lexer.Speed $$}
 spell {Lexer.Token Lexer.Spell (_)}
 start {Lexer.Token Lexer.StartSkill (_)}
 stretch {Lexer.Token Lexer.Stretch (_)}
 sum {Lexer.Token Lexer.Sum (_)}
 then {Lexer.Token Lexer.Then (_)}
 thought {Lexer.Token Lexer.Thought (_)}
 thoughts {Lexer.Token Lexer.Thoughts (_)}
 unable {Lexer.Token Lexer.Unable (_)}
 union {Lexer.Token Lexer.Union (_)}
 unit {Lexer.Token Lexer.Unit (_)}
 var {Lexer.Token (Lexer.Identifier $$) (_)}
 where {Lexer.Token Lexer.Where (_)}
 word {Lexer.Token (Lexer.Word _) $$}
 {-else {Lexer.Token Lexer.Else (_)}-}
 {-lex_error {Lexer.Token (Lexer.Error $$) (_)} {- WAIT A MINUTE... I DON'T WANT THIS!!! -}-}
 {-string {Lexer.Token (Lexer.TargetString $$) (_)}-}
{- eof {Lexer.Token Lexer.EOFToken (_)} {- NO NO NO I do not want this either!!!!! -}-}
   
   
   


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

File : Units Spells {File dummySurfaceData $1 $2}
Units : {[]}
      | Unit Units {$1 : $2}
Spells : {[]}
       | Spell Spells {$1 : $2}
Unit : unit name Stats Start End Counter Spawn Death Auto Actions Soul {Unit dummySurfaceData $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 {-Should make sure in the type checker that the list of LValues is nonempty-}}
Spell : spell name School level colon number spawn colon Skill {Spell dummySurfaceData $2 $3 $6 $9}
Stats : Schools level colon number hp colon number attack colon number defense colon number speed colon number range colon number soulPoints colon number {Stats dummySurfaceData $1 $4 $7 $10 $13 $16 $19 $22}
School : word {Knowledge $1}
Schools : {NoSchools dummySurfaceData }
        | word {OneSchool $1 $1}
        | word word {TwoSchools dummySurfaceData $1 $2}
Start : {Nothing}
      | start colon Skill {Just $ Start dummySurfaceData $3}
End : {Nothing}
    | end colon Skill {Just $ End dummySurfaceData $3}
Counter : {Nothing}
        | counter colon Skill {Just $ Counter dummySurfaceData $3}
Spawn : {Nothing}
      | spawn colon Skill {Just $ Spawn dummySurfaceData $3}
Death : {Nothing}
      | death colon Skill {Just $ Death dummySurfaceData $3}
Auto : {Nothing}
     | auto colon Skill {Just $ Auto dummySurfaceData $3}
Actions : {[]}
        | Action Actions {$1 : $2}
Action : action colon Skill {Action dummySurfaceData $3}
Soul : soul colon Skill {Soul dummySurfaceData $3}
Skill : OptionalCost OptionalCondition Automatic {AutomaticSkill dummySurfaceData (Just $1) $2 $3}
OptionalCost : {Constant dummySurfaceData "0" {-it does not make sense to have surface data here.... as there is no syntax...-}}
             | cost colon number {Constant dummySurfaceData "LALALA"}
OptionalCondition : {undefined}
                  | condition colon Expr {undefined}
OptionalFilter : {Always dummySurfaceData}
               | where Expr {$2}
Nonautomatic : {TerminatedSkillComponent}
             | select SelectionStatement NullableExpr ThenCase IfUnableCase NextAutomatic {Nonautomatic dummySurfaceData $2 $3 $4 $5 $6}
Automatic : SkillEffects Nonautomatic {Automatic dummySurfaceData $1 $2 {-Ignoring Universal case for now-} }
          | for each var in Side RelativeSet OptionalFilter comma {undefined {-Only allow one universally quantified variable at once. No pairs -}}
ThenCase : then lbracket Automatic rbracket {$3}
IfUnableCase : {Automatic dummySurfaceData [] (TerminatedSkillComponent {-NOT PART OF SURFACE SYNTAX... maybe terminated should never be part of surface...-})}
             | if unable lbracket Automatic rbracket {$4}
NextAutomatic : {Automatic dummySurfaceData [] (TerminatedSkillComponent)}
              | lbracket Automatic rbracket {$2}
SelectionStatement : Variables in Set RestSelectionStatement {[] ++ $4}
Set : Side RelativeSet {SimpleSet dummySurfaceData $1 $2}
    | Set union Set {UnionSet dummySurfaceData $1 $3}
RestSelectionStatement : {[]}
                       | comma Variables in Set RestSelectionStatement { (getFoo $2 $4) ++ $5}
Variables : var RestVariable {$1 : $2}
RestVariable : {[]}
             | comma var RestVariable {$2 : $3}
SkillEffects : {[]}
             | SkillEffect semicolon SkillEffects {$1 : $3}

SkillEffect : Assignment {$1}
Assignment : lparen ListExpr rparen Mutator Expr {Assignment dummySurfaceData $2 $4 $5}
Mutator : assign {Set dummySurfaceData}
        | increment {Increment dummySurfaceData}
        | decrement {Decrement dummySurfaceData}
        | stretch {Stretch dummySurfaceData}
        | crush {Crush dummySurfaceData}
        | contort {Contort dummySurfaceData}
ListExpr : {[]}
         | Expr {[$1]}
         | Expr comma ListExprCommas {$1 : $3}
ListExprCommas : Expr {[$1]}
               | Expr comma ListExprCommas {$1 : $3}
NullableExpr : {Nothing}
             | Expr {Just ($1)}
Expr : number {Constant dummySurfaceData "LLALALA"}
     | Field self {Self dummySurfaceData $1}
     | Field var {Var dummySurfaceData $1 $2}
     | Side School {KnowledgeExpr dummySurfaceData $2 $1}
     | Side thoughts {ThoughtsExpr dummySurfaceData $1 {-CURRENTLY DO NOT HAVE ERROR MESSAGE IF PLURALITY WRONG-}}
     | Side thought {ThoughtsExpr dummySurfaceData $1 }
     | Expr sum Expr {Sum dummySurfaceData $1 $3}
     | Expr difference Expr {Difference dummySurfaceData $1 $3}
     | Expr product Expr {Product dummySurfaceData $1 $3}
     | Expr quotient Expr {Quotient dummySurfaceData $1 $3}
     | Expr mod Expr {Mod dummySurfaceData $1 $3} 
     | Expr gt Expr {Parser.GT dummySurfaceData $1 $3}
     | Expr geq Expr {GEQ dummySurfaceData $1 $3}
     | Expr lt Expr {Parser.LT dummySurfaceData $1 $3}
     | Expr leq Expr {LEQ dummySurfaceData $1 $3}
     | Expr eq Expr {Parser.EQ dummySurfaceData $1 $3}
     | Expr and Expr {And dummySurfaceData $1 $3}
     | Expr or Expr {Or dummySurfaceData $1 $3}
     | not Expr {Not dummySurfaceData $2}
Field : Stat Temporality {StatField dummySurfaceData $1 $2}
      | HpStat {HpStatField dummySurfaceData $1}
      | engagement {EngagementField dummySurfaceData}  
Temporality : current {Temporary dummySurfaceData }
            | permanent {Permanent dummySurfaceData }
            | base {Base dummySurfaceData}
HpStat : hp {CurrentHp dummySurfaceData}
       | max hp {MaxHp dummySurfaceData}
       | base hp {BaseHp dummySurfaceData}
Stat : attack {Attack dummySurfaceData}
     | defense {Defense dummySurfaceData}
     | speed {Speed dummySurfaceData}
     | range {Range dummySurfaceData}
     | level {Level dummySurfaceData}
Side : friendly {Friendly dummySurfaceData}
     | enemy {Enemy dummySurfaceData}
RelativeSet : field {Field dummySurfaceData}
            | hand {Hand dummySurfaceData}
            | graveyard {Graveyard dummySurfaceData}
            | banished {Banished dummySurfaceData}
            | spawn {SpawnLocation dummySurfaceData}


{


dummySurfaceData :: Lexer.SurfaceData
dummySurfaceData = Lexer.SurfaceData (-1) (-1) "Dummy Surface Syntax"



getFileSurfaceData :: [Unit] -> [Spell] -> Lexer.SurfaceData
getFileSurfaceData = undefined


getFoo :: [String] -> Set -> [(String,Set)]
getFoo _ _ = error "what is getFoo??"

type P a = Lexer.Alex a



thenP = (>>=)

returnP = return
{-
failP = fail
-}
catchP m c = fail "catch not implemented"







lexer :: (Lexer.Token -> P a) -> P a
lexer = (Lexer.alexMonadScan >>=)


parseError tokens = do
 i <- Lexer.getLineNumber
 Lexer.alexError $ show i



data File = File Lexer.SurfaceData [Unit] [Spell]
             deriving Show

data Unit = Unit Lexer.SurfaceData String Stats (Maybe Start) (Maybe End) (Maybe Counter) (Maybe Spawn) (Maybe Death) (Maybe Auto) [Action] Soul
            deriving Show
data Spell = Spell Lexer.SurfaceData String Knowledge Lexer.SurfaceData Skill {-name, school, level, skill-}
             deriving Show

data Skill = AutomaticSkill Lexer.SurfaceData (Maybe Expr) Expr Automatic
        {-   | NonautomaticSkill Expr Expr Nonautomatic -}
           deriving Show
{-Cost, Condition, skill-}



{-

This is one place where syntatic sugar becomes interesting.

I am creating a data structure which captures the surface level syntax, even though this is a terrible of representing the data for other purposes.

This allows for better errors messages to be printed.


-}


data VariableBindings = VariableBindings Lexer.SurfaceData [([String], Set)]


data Side = Friendly Lexer.SurfaceData
          | Enemy Lexer.SurfaceData
          deriving Show
            

data RelativeSet = Field Lexer.SurfaceData
                 | Hand Lexer.SurfaceData
                 | Graveyard Lexer.SurfaceData
                 | Banished Lexer.SurfaceData
                 | SpawnLocation Lexer.SurfaceData
                 deriving Show


data Set = SimpleSet Lexer.SurfaceData Side RelativeSet
         | UnionSet Lexer.SurfaceData Set Set
         deriving Show

{-
Don't need both condition and nullable expression.


Either skill effects, or automatic should be allowed to have conditions and/or ifs.

-}
data SkillEffect = Assignment Lexer.SurfaceData [Expr] Mutator Expr
                 deriving Show

data Nonautomatic = Nonautomatic Lexer.SurfaceData [(String, Set)] (Maybe Expr) Automatic Automatic Automatic {-variables, where condition-}
                  | TerminatedSkillComponent
                  deriving Show
data Automatic = Automatic Lexer.SurfaceData [SkillEffect] Nonautomatic
               deriving Show

data Stats = Stats Lexer.SurfaceData Schools Lexer.SurfaceData Lexer.SurfaceData Lexer.SurfaceData Lexer.SurfaceData Lexer.SurfaceData Lexer.SurfaceData Lexer.SurfaceData
           deriving Show
data Start = Start Lexer.SurfaceData Skill
           deriving Show
data End = End Lexer.SurfaceData Skill
           deriving Show
data Counter = Counter Lexer.SurfaceData Skill
            deriving Show
data Spawn = Spawn Lexer.SurfaceData Skill
            deriving Show
data Death = Death Lexer.SurfaceData Skill
            deriving Show
data Auto = Auto Lexer.SurfaceData Skill
           deriving Show
data Action = Action Lexer.SurfaceData Skill
            deriving Show
data Soul = Soul Lexer.SurfaceData Skill
           deriving Show

data Stat = Attack Lexer.SurfaceData
          | Defense Lexer.SurfaceData 
          | Speed Lexer.SurfaceData
          | Range Lexer.SurfaceData
          | Level Lexer.SurfaceData 
          deriving Show
data Mutator = Increment Lexer.SurfaceData
             | Decrement Lexer.SurfaceData
             | Stretch Lexer.SurfaceData
             | Crush Lexer.SurfaceData
             | Contort Lexer.SurfaceData
             | Set Lexer.SurfaceData
             deriving Show
data Temporality = Temporary Lexer.SurfaceData
                 | Permanent Lexer.SurfaceData
                 | Base Lexer.SurfaceData
                 deriving Show
data HpStat = CurrentHp Lexer.SurfaceData
            | MaxHp Lexer.SurfaceData
            | BaseHp Lexer.SurfaceData
            deriving Show
data Engagement = Engagement Lexer.SurfaceData
                deriving Show




data Knowledge = Knowledge Lexer.SurfaceData
               deriving Show


data Schools = NoSchools Lexer.SurfaceData {-tricky to have surface data here as it's nullable...-}
             | OneSchool Lexer.SurfaceData Lexer.SurfaceData
             | TwoSchools Lexer.SurfaceData Lexer.SurfaceData Lexer.SurfaceData
             deriving Show


data Field = StatField Lexer.SurfaceData Stat Temporality
           | HpStatField Lexer.SurfaceData HpStat
           | EngagementField Lexer.SurfaceData
           deriving Show

{-Effects that happen to two units simultaneously trigger resulting effects by order of field position, with
ties broken by initiative. That is the significance of assigning two values at the "same" time-}


data Expr = Constant Lexer.SurfaceData [Char]
          | ThoughtsExpr Lexer.SurfaceData Side
          | KnowledgeExpr Lexer.SurfaceData Knowledge Side
          | Self Lexer.SurfaceData Field
          | Var Lexer.SurfaceData Field String
          | Sum Lexer.SurfaceData Expr Expr
          | Difference Lexer.SurfaceData Expr Expr
          | Product Lexer.SurfaceData Expr Expr
          | Quotient Lexer.SurfaceData Expr Expr
          | Mod Lexer.SurfaceData Expr Expr
          | Always Lexer.SurfaceData {-add more booleans later.....    again... nullable.-}
          | GT Lexer.SurfaceData Expr Expr
          | GEQ Lexer.SurfaceData Expr Expr
          | LT Lexer.SurfaceData Expr Expr
          | LEQ Lexer.SurfaceData Expr Expr
          | EQ Lexer.SurfaceData Expr Expr
          | And Lexer.SurfaceData Expr Expr
          | Or Lexer.SurfaceData Expr Expr
          | Not Lexer.SurfaceData Expr
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
extractSurface (Lexer.Token _ (Lexer.SurfaceData _ _ s)) = s

prettyPrint :: [String] -> String
prettyPrint [] = ""
prettyPrint (x:[]) = x
prettyPrint (x1:x2:xs) = x1 ++ " " ++ (prettyPrint (x2:xs))




generateTokenLocation :: [Lexer.Token] -> HashMap.Map (Int,Int) String
generateTokenLocation [] = HashMap.empty
generateTokenLocation ((Lexer.Token tokenValue (Lexer.SurfaceData line column surface)):xs) = HashMap.insert (line,column) surface (generateTokenLocation xs)


}

