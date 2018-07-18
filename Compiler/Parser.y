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
 as {Lexer.Token Lexer.LexerAs (_)}
 assign {Lexer.Token Lexer.Assignment (_)}
 attack {Lexer.Token Lexer.Attack $$}
 auto {Lexer.Token Lexer.AutoSkill (_)}
 banished {Lexer.Token Lexer.Banished (_)}
 base {Lexer.Token Lexer.Base (_)}
 behind {Lexer.Token Lexer.LexerBehind (_)}
 cardinality {Lexer.Token Lexer.Cardinality (_)}
 colon {Lexer.Token Lexer.Colon (_)}
 comma {Lexer.Token Lexer.Comma (_)}
 condition {Lexer.Token Lexer.Condition (_)}
 contort {Lexer.Token Lexer.Contort (_)}
 cost {Lexer.Token Lexer.Cost (_)}
 counter {Lexer.Token Lexer.CounterSkill (_)}
 crush {Lexer.Token Lexer.Crush (_)}
 current {Lexer.Token Lexer.Temporary (_)}
 damage {Lexer.Token Lexer.Damage (_)}
 dead {Lexer.Token Lexer.Dead (_)}
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
 front {Lexer.Token Lexer.LexerFront (_)}
 geq {Lexer.Token Lexer.GEQ (_)}
 graveyard {Lexer.Token Lexer.Graveyard (_)}
 gt {Lexer.Token Lexer.Gt (_)}
 hand {Lexer.Token Lexer.Hand (_)}
 hp {Lexer.Token Lexer.Hp $$}
 if {Lexer.Token Lexer.If (_)}
 in {Lexer.Token Lexer.In (_)}
 increment {Lexer.Token Lexer.Increment (_)}
 lbracket {Lexer.Token Lexer.Lbracket (_)}
 left {Lexer.Token Lexer.LexerLeft (_)}
 leq {Lexer.Token Lexer.LEQ (_)}
 level {Lexer.Token Lexer.Level (_)}
 lparen  {Lexer.Token Lexer.Lparen (_)}
 lt {Lexer.Token Lexer.Lt (_)}
 max {Lexer.Token Lexer.Max (_)}
 mod {Lexer.Token Lexer.Mod (_)}
 name {Lexer.Token (Lexer.TargetString $$) (_)}
 not {Lexer.Token Lexer.Not (_)}
 number {Lexer.Token (Lexer.Number _) $$  {-This is going to need a projection function probably??-}}
 of {Lexer.Token Lexer.LexerOf (_)}
 on {Lexer.Token Lexer.LexerOn (_)}
 or {Lexer.Token Lexer.Or (_)}
 permanent {Lexer.Token Lexer.Permanent (_)}
 position {Lexer.Token Lexer.LexerPosition (_)}
 product {Lexer.Token Lexer.Product (_)}
 quotient {Lexer.Token Lexer.Quotient (_)}
 range {Lexer.Token Lexer.Range $$}
 rbracket {Lexer.Token Lexer.Rbracket (_)}
 revive {Lexer.Token Lexer.Revive (_)}
 right {Lexer.Token Lexer.LexerRight (_)}
 rparen {Lexer.Token Lexer.Rparen (_)}
 same {Lexer.Token Lexer.LexerSame (_)}
 select {Lexer.Token Lexer.Select (_)}
 self {Lexer.Token Lexer.Self (_)}
 semicolon {Lexer.Token Lexer.Semicolon (_)}
 send {Lexer.Token Lexer.Send (_)}
 soul {Lexer.Token Lexer.SoulSkill (_)}
 soulPoints {Lexer.Token Lexer.SoulPoints (_)}
 spawn {Lexer.Token Lexer.SpawnSkill (_)}
 speed {Lexer.Token Lexer.Speed $$}
 spell {Lexer.Token Lexer.Spell (_)}
 square {Lexer.Token Lexer.LexerSquare (_)}
 start {Lexer.Token Lexer.StartSkill (_)}
 stretch {Lexer.Token Lexer.Stretch (_)}
 sum {Lexer.Token Lexer.Sum (_)}
 the {Lexer.Token Lexer.LexerThe (_)}
 then {Lexer.Token Lexer.Then (_)}
 this {Lexer.Token Lexer.LexerThis (_)}
 thought {Lexer.Token Lexer.Thought (_)}
 thoughts {Lexer.Token Lexer.Thoughts (_)}
 to {Lexer.Token Lexer.To (_)}
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
OptionalCost : {Nothing} -- OPTIONAL COSTS SHOULD ALLOW ANY EXPRESSION FOR THE COST NOT JUST A NUMBER (right now it's just a number)
             | cost colon number {Just $ CarryingSource dummySurfaceData $ Constant (getSurfaceContents $3)}
OptionalCondition : {Nothing}
                  | condition colon Expr {Just $3}
OptionalFilter : {CarryingSource dummySurfaceData Always}
               | where Expr {$2}
Nonautomatic : {TerminatedSkillComponent} -- the nullableexpr needs to be changed to be prefixed by where syntax.
             | select SelectionStatement NullableExpr ThenCase IfUnableCase NextAutomatic {Nonautomatic dummySurfaceData $2 $3 $4 $5 $6}
             | comma Automatic {Next $2}
Automatic : SkillEffects Nonautomatic {Automatic dummySurfaceData $1 $2 }
          | for each var in Set OptionalFilter comma SkillEffects Nonautomatic {Universal dummySurfaceData ($3,$5) $6 $8 $9 {-Only allow one universally quantified variable at once. No pairs -}}



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
             | if condition then SkillEffect semicolon SkillEffects {undefined}
-- skill effect needs to be able to include conditionals. This is because
-- I am executing lists of skill effects, and conditions can change between skill effects in the list.
-- I also want to be able to do universal quantification of the form
-- do 50 damage to x. for all units with less than 0 hp, ....
SkillEffect : Assignment {$1}
            | Revive {$1}
            | Damage {$1}
            | SendVarToGraveyard {$1}
            | SendSelfToGraveyard {$1}
            | SendSquareToGraveyard {$1}
Assignment : lparen ListExpr rparen Mutator Expr {Assignment dummySurfaceData $2 $4 $5}
Revive : revive var {Revive (extractSurfaceData $1) $2}
Damage : damage var Expr {DamageVar dummySurfaceData $2 $3 }
       | damage self Expr {DamageSelf dummySurfaceData $3 }
       | damage Side unit on position number word Expr {DamageSquare dummySurfaceData undefined undefined undefined}
       | damage unit on the same square as var Expr {DamageSameSquareVar dummySurfaceData $8 $9}
       | damage unit to the left of var Expr {undefined}
       | damage unit to the right of var Expr {undefined}
       | damage unit in front of var Expr {undefined}
       | damage unit behind var Expr {undefined}
       | damage unit on the same square as this unit Expr {undefined}
       | damage unit to the left of this unit Expr {undefined}
       | damage unit to the right of this unit Expr {undefined}
       | damage unit in front of this unit Expr {undefined}
       | damage unit behind this unit Expr {undefined}

{-
 = Assignment Lexer.SurfaceData [CarryingSource Expr] Mutator (CarryingSource Expr)
 | Revive Lexer.SurfaceData String
 | DamageSelf Lexer.SurfaceData (CarryingSource Expr)
 | DamageVar Lexer.SurfaceData String (CarryingSource Expr)
 | SendVarToGraveyard Lexer.SurfaceData String
 | SendSelfToGraveyard Lexer.SurfaceData
 | DamageSquare Lexer.SurfaceData Side String (CarryingSource Expr)
 | DamageSameSquareSelf Lexer.SurfaceData (CarryingSource Expr)
 | DamageSameSquareVar Lexer.SurfaceData String (CarryingSource Expr)
 | DamageLeftSelf Lexer.SurfaceData (CarryingSource Expr)
 | DamageRightSelf Lexer.SurfaceData (CarryingSource Expr)
 | DamageBehindSelf Lexer.SurfaceData (CarryingSource Expr)
 | DamageInFrontOfSelf Lexer.SurfaceData (CarryingSource Expr)
 | DamageLeftVar Lexer.SurfaceData String (CarryingSource Expr)
 | DamageRightVar Lexer.SurfaceData String (CarryingSource Expr)
 | DamageBehindVar Lexer.SurfaceData String (CarryingSource Expr)
 | DamageInFrontOfVar Lexer.SurfaceData String (CarryingSource Expr)
-}




SendVarToGraveyard : send var to graveyard {SendVarToGraveyard dummySurfaceData $2}
SendSelfToGraveyard : send self to graveyard {SendSelfToGraveyard dummySurfaceData}
SendSquareToGraveyard : send Side unit on position number to graveyard {undefined}
                      | send to graveyard unit on the same square as var {undefined}
                      | send to graveyard unit to the left of var {undefined}
                      | send to graveyard unit to the right of var {undefined}
                      | send to graveyard unit in front of var {undefined}
                      | send to graveyard unit behind var {undefined}
                      | send to graveyard unit on the same square as this unit {undefined}
                      | send to graveyard unit to the left of this unit {undefined}
                      | send to graveyard unit to the right of this unit {undefined}
                      | send to graveyard unit in front of this unit {undefined}
                      | send to graveyard unit behind this unit {undefined}



{-
use this for field above, and also for damage in effects
data FieldLocation
 = OneFL
 | TwoFL
 | ThreeFL
 | FourFL
 | FiveFL
 | SixFL
 | SevenFL
 | EightFL
 | NineFL
 | OnSameSquare Variable -- originally given side; typechecker will make sure variable is the opposite side as the side data listed here.
 | ToTheLeftOf Variable
 | ToTheRightOf Variable
 | InFrontOf Variable
 | Behind Variable
 | OnSameSquareSelf -- refers to enemy of course
 | ToTheLeftOfSelf -- this refers to friendly...
 | ToTheRightOfSelf
 | InFrontOfSelf
 | BehindSelf
 deriving Show

 | DamageSelf Lexer.SurfaceData
 | DamageVar Lexer.SurfaceData String

-}
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
     | dead var {CarryingSource dummySurfaceData $ Dead $2}
     | Side School {CarryingSource dummySurfaceData $ KnowledgeExpr $2 $1}
     | Side thoughts {CarryingSource (extractSurfaceData $2) $ ThoughtsExpr $1 {-CURRENTLY DO NOT HAVE ERROR MESSAGE IF PLURALITY WRONG-}}
     | Side thought {CarryingSource (extractSurfaceData $2) $ ThoughtsExpr $1 }
     | Expr sum Expr {CarryingSource (unionSurfaceData (getSurfaceData' $1) (unionSurfaceData (extractSurfaceData $2) (getSurfaceData' $3))) $ Sum $1 $3}
     | Expr difference Expr {CarryingSource (unionSurfaceData (getSurfaceData' $1) (unionSurfaceData (extractSurfaceData $2) (getSurfaceData' $3))) $ Difference $1 $3}
     | Expr product Expr {CarryingSource (unionSurfaceData (getSurfaceData' $1) (unionSurfaceData (extractSurfaceData $2) (getSurfaceData' $3))) $ Product $1 $3}
     | Expr quotient Expr {CarryingSource (unionSurfaceData (getSurfaceData' $1) (unionSurfaceData (extractSurfaceData $2) (getSurfaceData' $3))) $ Quotient $1 $3}
     | Expr mod Expr {CarryingSource (unionSurfaceData (getSurfaceData' $1) (unionSurfaceData (extractSurfaceData $2) (getSurfaceData' $3))) $ Mod $1 $3} 
     | Expr gt Expr {CarryingSource (unionSurfaceData (getSurfaceData' $1) (unionSurfaceData (extractSurfaceData $2) (getSurfaceData' $3))) $ ParseTree.GT $1 $3}
     | Expr geq Expr {CarryingSource (unionSurfaceData (getSurfaceData' $1) (unionSurfaceData (extractSurfaceData $2) (getSurfaceData' $3))) $ GEQ $1 $3} -- currently missing surface data from the subtrees.
     | Expr lt Expr {CarryingSource (unionSurfaceData (getSurfaceData' $1) (unionSurfaceData (extractSurfaceData $2) (getSurfaceData' $3))) $ ParseTree.LT $1 $3}
     | Expr leq Expr {CarryingSource (unionSurfaceData (getSurfaceData' $1) (unionSurfaceData (extractSurfaceData $2) (getSurfaceData' $3))) $ LEQ $1 $3}
     | Expr eq Expr {CarryingSource (unionSurfaceData (getSurfaceData' $1) (unionSurfaceData (extractSurfaceData $2) (getSurfaceData' $3))) $ ParseTree.EQ $1 $3}
     | Expr and Expr {CarryingSource (unionSurfaceData (getSurfaceData' $1) (unionSurfaceData (extractSurfaceData $2) (getSurfaceData' $3))) $ And $1 $3}
     | Expr or Expr {CarryingSource (unionSurfaceData (getSurfaceData' $1) (unionSurfaceData (extractSurfaceData $2) (getSurfaceData' $3))) $ Or $1 $3}
     | not Expr {CarryingSource (unionSurfaceData (extractSurfaceData $1) (getSurfaceData' $2)) $ Not $2}
     | word {-should change word so it has to be a school-} Side {CarryingSource (dummySurfaceData) $ KnowledgeExpr (Knowledge $1) $2}
     | self in range var {CarryingSource dummySurfaceData $ SelfInRangeVar $4}
     | var in range var {CarryingSource dummySurfaceData $ VarInRangeVar $1 $4}
     | var in range self {CarryingSource dummySurfaceData $ VarInRangeSelf $1}
     | cardinality lparen var in Set where Expr rparen {CarryingSource dummySurfaceData $ Cardinality $3 $5 $7}
     | cardinality lparen var in Set rparen {CarryingSource dummySurfaceData $ Cardinality $3 $5 (CarryingSource dummySurfaceData Always)}
     | Field unit in Side position number {undefined}
     | Field unit on the same square as var {undefined}
     | Field unit to the left of var {undefined}
     | Field unit to the right of var {undefined}
     | Field unit in front of var {undefined}
     | Field unit behind var {undefined}
     | Field unit on the same square as this unit {undefined}
     | Field unit to the left of this unit {undefined}
     | Field unit to the right of this unit {undefined}
     | Field unit in front of this unit {undefined}
     | Field unit behind this unit {undefined}
     | lparen Expr rparen {$2}

{-
use this for field above, and also for damage in effects
data FieldLocation
 = OneFL
 | TwoFL
 | ThreeFL
 | FourFL
 | FiveFL
 | SixFL
 | SevenFL
 | EightFL
 | NineFL
 | OnSameSquare Variable -- originally given side; typechecker will make sure variable is the opposite side as the side data listed here.
 | ToTheLeftOf Variable
 | ToTheRightOf Variable
 | InFrontOf Variable
 | Behind Variable
 | OnSameSquareSelf -- refers to enemy of course
 | ToTheLeftOfSelf -- this refers to friendly...
 | ToTheRightOfSelf
 | InFrontOfSelf
 | BehindSelf
 deriving Show


-- Stuff for affecting entire rows or columns to be dealt with elsewhere.
-- This just indexes a particular square.
-------------------------------------------------------------------------------
data LExpr
 = LThoughtsExpr SurfaceData ParseTree.Side
 | LKnowledgeExpr SurfaceData Knowledge ParseTree.Side
 | LSelfProjection SurfaceData LStat {-should exclude base stats, soul points...-}
 | LVarProjection SurfaceData Variable LStat
 | LFieldProjection SurfaceData FieldLocation


-}


--| RCardinality SurfaceData RBool ParseTree.Set
--| Cardinality String Set Expr
     {- x in range y means that y can target x -}
Field : Temporality Stat {StatField dummySurfaceData $2 $1}
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



-- move the following code eventually
getSurfaceContents :: Lexer.SurfaceData -> String
getSurfaceContents (Lexer.SurfaceData _ _ contents) = contents
}

