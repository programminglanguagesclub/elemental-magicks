{
module Parser where    
import qualified Lexer
import ParseTree

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

File : Units Spells {File $1 $2}
Units : {[]}
      | Unit Units {$1 : $2}
Spells : {[]}
       | Spell Spells {$1 : $2}
Unit : unit name Stats Start End Counter Spawn Death Auto Actions Soul {Unit $2 (CarryingSource (Lexer.SurfaceData 1 1 "dummy") $3) $4 $5 $6 $7 $8 $9 $10 $11 {-Should make sure in the type checker that the list of LValues is nonempty-}}
Spell : spell name School level colon number spawn colon Skill {Spell dummySurfaceData $2 $3 $6 $9}
Stats : Schools level colon number hp colon number attack colon number defense colon number speed colon number range colon number soulPoints colon number {Stats dummySurfaceData $1 $4 $7 $10 $13 $16 $19 $22}
School : word {Knowledge $1}
Schools : {NoSchools}
        | word {OneSchool $1 $1}
        | word word {TwoSchools (unionSurfaceData $1 $2) $1 $2}
Start : {Nothing}
      | start colon Skill {Just $ CarryingSource (getSurfaceData' $3) $ Start $3}
End : {Nothing}
    | end colon Skill {Just $ CarryingSource (getSurfaceData' $3) $ End $3}
Counter : {Nothing}
        | counter colon Skill {Just $ CarryingSource (getSurfaceData' $3) $ Counter $3}
Spawn : {Nothing}
      | spawn colon Skill {Just $ CarryingSource (getSurfaceData' $3) $ Spawn $3}
Death : {Nothing}
      | death colon Skill {Just $ CarryingSource (getSurfaceData' $3) $ Death $3}
Auto : {Nothing}
     | auto colon Skill {Just $ CarryingSource (getSurfaceData' $3) $ Auto $3}
Actions : {[]}
        | Action Actions {$1 : $2}
Action : action colon Skill {CarryingSource (getSurfaceData' $3) $ Action $3}
Soul : soul colon Skill {CarryingSource dummySurfaceData $ Soul $3}
Skill : OptionalCost OptionalCondition Automatic {CarryingSource dummySurfaceData $ AutomaticSkill $1 $2 $3}
OptionalCost : {Nothing} -- OPTIONAL COSTS SHOULD ALLOW ANY EXPRESSION FOR THE COST NOT JUST A NUMBER
             | cost colon number {Just $ CarryingSource dummySurfaceData $ Constant "LALALA"}
OptionalCondition : {Nothing}
                  | condition colon Expr {Just $3}
OptionalFilter : {CarryingSource dummySurfaceData Always}
               | where Expr {$2}
Nonautomatic : {TerminatedSkillComponent} -- the nullableexpr needs to be changed to be prefixed by where syntax.
             | select SelectionStatement NullableExpr ThenCase IfUnableCase NextAutomatic {Nonautomatic dummySurfaceData $2 $3 $4 $5 $6}
Automatic : SkillEffects Nonautomatic {Automatic dummySurfaceData $1 $2 {-Ignoring Universal case for now-} }
          | for each var in Side RelativeSet OptionalFilter comma {undefined {-Only allow one universally quantified variable at once. No pairs -}}
ThenCase : then lbracket Automatic rbracket {$3}
IfUnableCase : {Automatic dummySurfaceData [] (TerminatedSkillComponent {-NOT PART OF SURFACE SYNTAX... maybe terminated should never be part of surface...-})}
             | if unable lbracket Automatic rbracket {$4}
NextAutomatic : {Automatic dummySurfaceData [] (TerminatedSkillComponent)}
              | lbracket Automatic rbracket {$2}
SelectionStatement : Variables in Set RestSelectionStatement {(getFoo $1 $3) ++ $4}
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
Mutator : assign {Set $ extractSurfaceData $1}
        | increment {Increment $ extractSurfaceData $1}
        | decrement {Decrement $ extractSurfaceData $1}
        | stretch {Stretch $ extractSurfaceData $1}
        | crush {Crush $ extractSurfaceData $1}
        | contort {Contort $ extractSurfaceData $1}
ListExpr : {[]}
         | Expr {[$1]}
         | Expr comma ListExprCommas {$1 : $3}
ListExprCommas : Expr {[$1]}
               | Expr comma ListExprCommas {$1 : $3}
NullableExpr : {Nothing}
             | Expr {Just ($1)}
Expr : number {CarryingSource $1 $ Constant (getSurfaceSyntax $1)}
     | Field self {CarryingSource (extractSurfaceData $2) $ Self $1}
     | Field var {CarryingSource dummySurfaceData $ Var $1 $2}
     | Side School {CarryingSource dummySurfaceData $ KnowledgeExpr $2 $1}
     | Side thoughts {CarryingSource (extractSurfaceData $2) $ ThoughtsExpr $1 {-CURRENTLY DO NOT HAVE ERROR MESSAGE IF PLURALITY WRONG-}}
     | Side thought {CarryingSource (extractSurfaceData $2) $ ThoughtsExpr $1 }
     | Expr sum Expr {CarryingSource (extractSurfaceData $2) $ Sum $1 $3}
     | Expr difference Expr {CarryingSource (extractSurfaceData $2) $ Difference $1 $3}
     | Expr product Expr {CarryingSource (extractSurfaceData $2) $ Product $1 $3}
     | Expr quotient Expr {CarryingSource (extractSurfaceData $2) $ Quotient $1 $3}
     | Expr mod Expr {CarryingSource (extractSurfaceData $2) $ Mod $1 $3} 
     | Expr gt Expr {CarryingSource (extractSurfaceData $2) $ ParseTree.GT $1 $3}
     | Expr geq Expr {CarryingSource (extractSurfaceData $2) $ GEQ $1 $3} -- currently missing surface data from the subtrees.
     | Expr lt Expr {CarryingSource (extractSurfaceData $2) $ ParseTree.LT $1 $3}
     | Expr leq Expr {CarryingSource (extractSurfaceData $2) $ LEQ $1 $3}
     | Expr eq Expr {CarryingSource (extractSurfaceData $2) $ ParseTree.EQ $1 $3}
     | Expr and Expr {CarryingSource (extractSurfaceData $2) $ And $1 $3}
     | Expr or Expr {CarryingSource (extractSurfaceData $2) $ Or $1 $3}
     | not Expr {CarryingSource (dummySurfaceData) $ Not $2}
Field : Stat Temporality {StatField dummySurfaceData $1 $2}
      | HpStat {HpStatField dummySurfaceData $1}
      | engagement {EngagementField dummySurfaceData}  
Temporality : current {Temporary (extractSurfaceData $1)}
            | permanent {Permanent (extractSurfaceData $1)}
            | base {Base (extractSurfaceData $1)}
HpStat : hp {CarryingSource dummySurfaceData CurrentHp}
       | max hp {CarryingSource dummySurfaceData MaxHp}
       | base hp {CarryingSource dummySurfaceData BaseHp}
Stat : attack {CarryingSource $1 Attack}
     | defense {CarryingSource $1 Defense}
     | speed {CarryingSource $1 Speed}
     | range {CarryingSource $1 Range}
     | level {CarryingSource (extractSurfaceData $1) Level}
Side : friendly {Friendly (extractSurfaceData $1)}
     | enemy {Enemy (extractSurfaceData $1)}
RelativeSet : field {Field (extractSurfaceData $1)}
            | hand {Hand (extractSurfaceData $1)}
            | graveyard {Graveyard (extractSurfaceData $1)}
            | banished {Banished (extractSurfaceData $1)}
            | spawn {SpawnLocation (extractSurfaceData $1)}


{

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


}

