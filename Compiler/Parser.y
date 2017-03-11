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
%lexer {lexer} {(Lexer.Token Lexer.EOFToken (_) (_) (_))}


%token
 action {Lexer.Token Lexer.ActionSkill (_) (_) (_)}
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
 end {Lexer.Token Lexer.EndSkill (_) (_) (_)}
 enemy {Lexer.Token Lexer.Enemy (_) (_) (_)}
 engagement {Lexer.Token Lexer.Engagement (_) (_) (_)}
 eq {Lexer.Token Lexer.Equality (_) (_) (_)}
 field {Lexer.Token Lexer.Field (_) (_) (_)}
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

File : Units Spells {File undefined $1 $2}
Units : {[]}
      | Unit Units {$1 : $2}
Spells : {[]}
       | Spell Spells {$1 : $2}
Unit : unit name Stats Start End Counter Spawn Death Auto Actions Soul {Unit undefined $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 {-Should make sure in the type checker that the list of LValues is nonempty-}}
Spell : spell name School level colon number spawn colon Skill {Spell undefined $2 $3 $6 $9}
Stats : Schools level colon number hp colon number attack colon number defense colon number speed colon number range colon number soulPoints colon number {Stats undefined $1 $4 $7 $10 $13 $16 $19 $22}
School : word {Knowledge undefined $1}
Schools : {NoSchools undefined }
        | word {OneSchool undefined $1}
        | word word {TwoSchools undefined $1 $2}
Start : {Nothing}
      | start colon Skill {Just $ Start undefined $3}
End : {Nothing}
    | end colon Skill {Just $ End undefined $3}
Counter : {Nothing}
        | counter colon Skill {Just $ Counter undefined $3}
Spawn : {Nothing}
      | spawn colon Skill {Just $ Spawn undefined $3}
Death : {Nothing}
      | death colon Skill {Just $ Death undefined $3}
Auto : {Nothing}
     | auto colon Skill {Just $ Auto undefined $3}
Actions : {[]}
        | Action Actions {$1 : $2}
Action : action colon Skill {Action undefined $3}
Soul : soul colon Skill {Soul undefined $3}
Skill : OptionalCost OptionalCondition Automatic {AutomaticSkill undefined $1 $2 $3}
OptionalCost : {Constant undefined "0"}
             | cost colon number {Constant undefined $3}
OptionalCondition : {Always undefined }
                  | condition colon Expr {$3}
OptionalFilter : {Always undefined}
               | where Expr {$2}
Nonautomatic : {TerminatedSkillComponent undefined}
             | select SelectionStatement NullableExpr ThenCase IfUnableCase NextAutomatic {Nonautomatic undefined $2 $3 $4 $5 $6}
Automatic : SkillEffects Nonautomatic {Automatic undefined $1 $2 {-Ignoring Universal case for now-} }
          | for each var in Side RelativeSet OptionalFilter comma {undefined {-Only allow one universally quantified variable at once. No pairs -}}
ThenCase : then lbracket Automatic rbracket {$3}
IfUnableCase : {Automatic undefined [] (TerminatedSkillComponent undefined {-NOT PART OF SURFACE SYNTAX... maybe terminated should never be part of surface...-})}
             | if unable lbracket Automatic rbracket {$4}
NextAutomatic : {Automatic undefined [] (TerminatedSkillComponent undefined)}
              | lbracket Automatic rbracket {$2}
SelectionStatement : Variables in Set RestSelectionStatement {[] ++ $4}
Set : Side RelativeSet {SimpleSet undefined $1 $2}
    | Set union Set {UnionSet undefined $1 $3}
RestSelectionStatement : {[]}
                       | comma Variables in Set RestSelectionStatement { (getFoo $2 $4) ++ $5}
Variables : var RestVariable {$1 : $2}
RestVariable : {[]}
             | comma var RestVariable {$2 : $3}
SkillEffects : {[]}
             | SkillEffect semicolon SkillEffects {$1 : $3}

SkillEffect : Assignment {$1}
Assignment : lparen ListExpr rparen Mutator Expr {Assignment undefined $2 $4 $5}
Mutator : assign {Set undefined}
        | increment {Increment undefined}
        | decrement {Decrement undefined}
        | stretch {Stretch undefined}
        | crush {Crush undefined}
        | contort {Contort undefined}
ListExpr : {[]}
         | Expr {[$1]}
         | Expr comma ListExprCommas {$1 : $3}
ListExprCommas : Expr {[$1]}
               | Expr comma ListExprCommas {$1 : $3}
NullableExpr : {Always undefined}
             | Expr {$1}
Expr : number {Constant undefined $1}
     | Field self {Self undefined $1}
     | Field var {Var undefined $1 $2}
     | Side School {KnowledgeExpr undefined $2 $1}
     | Side thoughts {ThoughtsExpr undefined $1 {-CURRENTLY DO NOT HAVE ERROR MESSAGE IF PLURALITY WRONG-}}
     | Side thought {ThoughtsExpr undefined $1 }
     | Expr sum Expr {Sum undefined $1 $3}
     | Expr difference Expr {Difference undefined $1 $3}
     | Expr product Expr {Product undefined $1 $3}
     | Expr quotient Expr {Quotient undefined $1 $3}
     | Expr mod Expr {Mod undefined $1 $3} 
     | Expr gt Expr {Parser.GT undefined $1 $3}
     | Expr geq Expr {GEQ undefined $1 $3}
     | Expr lt Expr {Parser.LT undefined $1 $3}
     | Expr leq Expr {LEQ undefined $1 $3}
     | Expr eq Expr {Parser.EQ undefined $1 $3}
     | Expr and Expr {And undefined $1 $3}
     | Expr or Expr {Or undefined $1 $3}
     | not Expr {Not undefined $2}
Field : Stat Temporality {StatField undefined $1 $2}
      | HpStat {HpStatField undefined $1}
      | engagement {EngagementField undefined}  
Temporality : current {Temporary undefined }
            | permanent {Permanent undefined }
            | base {Base undefined}
HpStat : hp {CurrentHp undefined}
       | max hp {MaxHp undefined}
       | base hp {BaseHp undefined}
Stat : attack {Attack undefined}
     | defense {Defense undefined}
     | speed {Speed undefined}
     | range {Range undefined}
     | level {Level undefined}
Side : friendly {Friendly undefined}
     | enemy {Enemy undefined}
RelativeSet : field {Field undefined}
            | hand {Hand undefined}
            | graveyard {Graveyard undefined}
            | banished {Banished undefined}
            | spawn {SpawnLocation undefined}


{

getFileSurfaceData :: [Unit] -> [Spell] -> SurfaceData
getFileSurfaceData = undefined


getFoo :: [String] -> Set -> [(String,Set)]
getFoo _ _ = undefined 

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


data SurfaceData = SurfaceData Int Int String
                 deriving Show


data File = File SurfaceData [Unit] [Spell]
             deriving Show

data Unit = Unit SurfaceData String Stats (Maybe Start) (Maybe End) (Maybe Counter) (Maybe Spawn) (Maybe Death) (Maybe Auto) [Action] Soul
            deriving Show
data Spell = Spell SurfaceData String Knowledge String Skill {-name, school, level, skill-}
             deriving Show

data Skill = AutomaticSkill SurfaceData Expr Expr Automatic
        {-   | NonautomaticSkill Expr Expr Nonautomatic -}
           deriving Show
{-Cost, Condition, skill-}



{-

This is one place where syntatic sugar becomes interesting.

I am creating a data structure which captures the surface level syntax, even though this is a terrible of representing the data for other purposes.

This allows for better errors messages to be printed.


-}


data VariableBindings = VariableBindings SurfaceData [([String], Set)]


data Side = Friendly SurfaceData
          | Enemy SurfaceData
          deriving Show
            

data RelativeSet = Field SurfaceData
                 | Hand SurfaceData
                 | Graveyard SurfaceData
                 | Banished SurfaceData
                 | SpawnLocation SurfaceData
                 deriving Show


data Set = SimpleSet SurfaceData Side RelativeSet
         | UnionSet SurfaceData Set Set
         deriving Show



data SkillEffect = Assignment SurfaceData [Expr] Mutator Expr
                 deriving Show

data Nonautomatic = Nonautomatic SurfaceData [(String, Set)] Expr Automatic Automatic Automatic {-variables, where condition-}
                  | TerminatedSkillComponent SurfaceData
                  deriving Show
data Automatic = Automatic SurfaceData [SkillEffect] Nonautomatic
               deriving Show

data Stats = Stats SurfaceData Schools String String String String String String String
           deriving Show
data Start = Start SurfaceData Skill
           deriving Show
data End = End SurfaceData Skill
           deriving Show
data Counter = Counter SurfaceData Skill
            deriving Show
data Spawn = Spawn SurfaceData Skill
            deriving Show
data Death = Death SurfaceData Skill
            deriving Show
data Auto = Auto SurfaceData Skill
           deriving Show
data Action = Action SurfaceData Skill
            deriving Show
data Soul = Soul SurfaceData Skill
           deriving Show

data Stat = Attack SurfaceData
          | Defense SurfaceData 
          | Speed SurfaceData
          | Range SurfaceData
          | Level SurfaceData 
          deriving Show
data Mutator = Increment SurfaceData
             | Decrement SurfaceData
             | Stretch SurfaceData
             | Crush SurfaceData
             | Contort SurfaceData
             | Set SurfaceData
             deriving Show
data Temporality = Temporary SurfaceData
                 | Permanent SurfaceData
                 | Base SurfaceData
                 deriving Show
data HpStat = CurrentHp SurfaceData
            | MaxHp SurfaceData
            | BaseHp SurfaceData
            deriving Show
data Engagement = Engagement SurfaceData
                deriving Show




data Knowledge = Knowledge SurfaceData String
               deriving Show


data Schools = NoSchools SurfaceData {-tricky to have surface data here as its nullable...-}
             | OneSchool SurfaceData String
             | TwoSchools SurfaceData String String
             deriving Show


data Field = StatField SurfaceData Stat Temporality
           | HpStatField SurfaceData HpStat
           | EngagementField SurfaceData
           deriving Show

{-Effects that happen to two units simultaneously trigger resulting effects by order of field position, with
ties broken by initiative. That is the significance of assigning two values at the "same" time-}


data Expr = Constant SurfaceData String
          | ThoughtsExpr SurfaceData Side
          | KnowledgeExpr SurfaceData Knowledge Side
          | Self SurfaceData Field
          | Var SurfaceData Field String
          | Sum SurfaceData Expr Expr
          | Difference SurfaceData Expr Expr
          | Product SurfaceData Expr Expr
          | Quotient SurfaceData Expr Expr
          | Mod SurfaceData Expr Expr
          | Always SurfaceData {-add more booleans later.....    again... nullable.-}
          | GT SurfaceData Expr Expr
          | GEQ SurfaceData Expr Expr
          | LT SurfaceData Expr Expr
          | LEQ SurfaceData Expr Expr
          | EQ SurfaceData Expr Expr
          | And SurfaceData Expr Expr
          | Or SurfaceData Expr Expr
          | Not SurfaceData Expr
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




generateTokenLocation :: [Lexer.Token] -> HashMap.Map (Int,Int) String
generateTokenLocation [] = HashMap.empty
generateTokenLocation ((Lexer.Token tokenValue line column surface):xs) = HashMap.insert (line,column) surface (generateTokenLocation xs)


}







