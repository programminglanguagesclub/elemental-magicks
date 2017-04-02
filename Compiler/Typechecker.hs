{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Typechecker where    
import qualified Lexer
import qualified Parser
import qualified ParseTree
import Lexer(SurfaceData)
import Text.Read
import Text.EditDistance
import Data.Monoid
import Control.Monad(join)
import Debug.Trace 
  
   

maxInt :: Int
maxInt = 1000
minInt :: Int
minInt = (-1000)

{-
levenshteinDistance

defaultEditCosts :: EditCosts

levenshteinDistance :: EditCosts -> String -> String -> Int

Find the Levenshtein edit distance between two strings. That is to say, the number of deletion, insertion and substitution operations that are required to make the two strings equal. Note that this algorithm therefore does not make use of the transpositionCost field of the costs. See also: http://en.wikipedia.org/wiki/Levenshtein_distance.

-}






data File = File SurfaceData [Unit] [Spell]
          deriving Show
data Unit = Unit SurfaceData String Stats (Maybe Start) (Maybe End) (Maybe Counter) (Maybe SpawnUnit) (Maybe Death) (Maybe Auto) [Action] Soul
          deriving Show
data Spell = Spell SurfaceData String Knowledge BaseLevel Skill {- name, school, level, skill -}
           deriving Show
data Skill = Skill SurfaceData (Maybe RInt) (Maybe RBool) Automatic {-Currently no check against this cost being negative. Also doesn't have to be a constant (design decision)-}
           deriving Show
data Start = Start SurfaceData Skill
           deriving Show
data End = End SurfaceData Skill
         deriving Show
data Counter = Counter SurfaceData Skill
             deriving Show
data SpawnUnit = SpawnUnit SurfaceData Skill
               deriving Show
data Death = Death SurfaceData Skill
           deriving Show
data Auto = Auto SurfaceData Skill
          deriving Show
data Action = Action SurfaceData Skill
            deriving Show
data Soul = Soul SurfaceData Skill
          deriving Show

data RInt = RConstant SurfaceData Int
          | RThoughts SurfaceData ParseTree.Side
          | RKnowledge SurfaceData Knowledge ParseTree.Side
          | RSelfProjection SurfaceData LStat {-disallow base for self. allow for var (because var can be quantified.-}
          | RVarProjection SurfaceData RStat Variable
          | RSum SurfaceData RInt RInt
          | RDifference SurfaceData RInt RInt
          | RProduct SurfaceData RInt RInt
          | RQuotient SurfaceData RInt RInt
          | RMod SurfaceData RInt RInt
          deriving Show
data RBool = RAlways SurfaceData
           | RGT SurfaceData RInt RInt
           | RGEQ SurfaceData RInt RInt
           | RLT SurfaceData RInt RInt
           | RLEQ SurfaceData RInt RInt
           | REQ SurfaceData RInt RInt
           | RAnd SurfaceData RBool RBool
           | ROr SurfaceData RBool RBool
           | RNot SurfaceData RBool
           deriving Show
data Knowledge = Earth SurfaceData
               | Fire SurfaceData
               | Water SurfaceData
               | Air SurfaceData
               | Spirit SurfaceData
               | Void SurfaceData
               deriving Show

instance Eq Knowledge where
 Earth _ == Earth _ = True
 Fire _ == Fire _ = True
 Water _ == Water _ = True
 Air _ == Air _ = True
 Spirit _ == Spirit _ = True
 Void _ == Void _ = True
 _ == _ = False





{-


data SkillEffect = SkillEffectAssignment SurfaceData Assignment {-I need more skill effects, of course-}
                 deriving Show
data Assignment = Assignment SurfaceData [LExpr] Mutator RInt
                deriving Show





from parser:
data SkillEffect = Assignment Lexer.SurfaceData [Expr] Mutator Expr
                 deriving Show
-}


typeCheckSkillEffect :: Context -> ParseTree.SkillEffect -> TC SkillEffect
typeCheckSkillEffect context skillEffect =
 case skillEffect of
  ParseTree.Assignment surfaceData lExprs mutator rExpr ->
   SkillEffectAssignment surfaceData
   <$> typeCheckAssignment context surfaceData lExprs mutator rExpr


typeCheckAssignment :: Context -> Lexer.SurfaceData -> [ParseTree.Expr] -> ParseTree.Mutator -> ParseTree.Expr -> TC Assignment
typeCheckAssignment context surfaceData lExprs mutator rExpr =
 Assignment surfaceData
 <$> traverse (typeCheckLInt context) lExprs
 <*> pure mutator
 <*> typeCheckRInt context rExpr






data Automatic = Automatic SurfaceData [SkillEffect] Nonautomatic
               deriving Show
data Nonautomatic = Selection SurfaceData [Judgment] (Maybe RBool) Automatic Automatic Automatic
                  deriving Show
data Stats = Stats SurfaceData Schools BaseLevel BaseHp BaseAttack BaseDefense BaseSpeed BaseRange BaseSoulPoints
           deriving Show
data Schools = NoSchools
             | EarthMono SurfaceData
             | FireMono SurfaceData
             | WaterMono SurfaceData
             | AirMono SurfaceData
             | SpiritMono SurfaceData
             | VoidMono SurfaceData
             | EarthFire SurfaceData
             | EarthWater SurfaceData
             | EarthAir SurfaceData
             | EarthSpirit SurfaceData
             | EarthVoid SurfaceData
             | FireWater SurfaceData
             | FireAir SurfaceData
             | FireSpirit SurfaceData
             | FireVoid SurfaceData
             | WaterAir SurfaceData
             | WaterSpirit SurfaceData
             | WaterVoid SurfaceData
             | AirSpirit SurfaceData
             | AirVoid SurfaceData
             | SpiritVoid SurfaceData
             deriving Show


data BaseLevel = BaseLevel SurfaceData Int
               deriving Show
data BaseHp = BaseHp SurfaceData Int
            deriving Show
data BaseAttack = BaseAttack SurfaceData Int
                deriving Show
data BaseDefense = BaseDefense SurfaceData Int
                 deriving Show
data BaseSpeed = BaseSpeed SurfaceData Int
               deriving Show
data BaseRange = BaseRange SurfaceData Int
               deriving Show
data BaseSoulPoints = BaseSoulPoints SurfaceData Int
                    deriving Show


{- bad -}
data Judgment = Judgment (Variable,ParseTree.Set)
               deriving Show


mkJudgment (string, set) =
 Judgment (Variable (Lexer.SurfaceData 0 0 "DUMMY") string, set)

data Variable = Variable SurfaceData String {-String of length 1-}
              deriving Show
data Context = EmptyContext
             | ExtendContext Context Judgment
             deriving Show





varIn :: Variable -> Context -> Bool
varIn var context =
 case context of
  EmptyContext -> False
  ExtendContext context' judgment ->
   (varMatches var var') || (varIn var context')
   where Judgment (var', _) = judgment

varMatches :: Variable -> Variable -> Bool
varMatches var1 var2 =
 varName1 == varName2
 where Variable _ varName1 = var1
       Variable _ varName2 = var2


{-does not display surface data in error message-}
tryVarPut :: Variable -> ParseTree.Set -> Context -> TC Context
tryVarPut _ _ EmptyContext = pure EmptyContext
tryVarPut var set (ExtendContext context judgment) =
 if varMatches var var2
  then TC $ Left $ ["variable " ++ varName ++ " already bound to " ++ (show set)]
  else pure $ ExtendContext (ExtendContext context judgment) $ Judgment (var,set)
 where (Judgment (var2, set)) = judgment
       (Variable _ varName) = var

tryExtendContext :: Context -> Judgment -> TC Context
tryExtendContext context judgment =
 tryVarPut variable set context
 where Judgment (variable, set) = judgment
 


tryExtendContextMultiple :: Context -> [Judgment] -> TC Context
tryExtendContextMultiple context [] = pure context
tryExtendContextMultiple context (x:xs) =
 joinTC $
 tryExtendContextMultiple
 <$> tryExtendContext context x
 <*> pure xs


{-
data Set = SimpleSet SurfaceData ParseTree.Side RelativeSet
         | UnionSet SurfaceData Set Set
         deriving Show
         {-
data Side = Friendly SurfaceData
          | Enemy SurfaceData
          deriving Show
-}
data RelativeSet = Field SurfaceData
                 | Hand SurfaceData
                 | Graveyard SurfaceData
                 | Banished SurfaceData
                 | SpawnLocation SurfaceData
                 deriving Show
-}
data LStat
 = LStatField Modifier LStatField
 | LHpStat LHpStat
 | LEngagement
 deriving Show


data Modifier
 = Temporary
 | Permanent
 | Base
 deriving Show
data LStatField
 = LAttack
 | LDefense
 | LSpeed
 | LRange
 | LLevel
 deriving Show

data LHpStat
 = LCurrentHp
 | LMaxHp
 | LBaseHp
 deriving Show


data RStat = RStat {-unimplemented. Like LStat but allows reference to base-}
           deriving Show


typeCheckLStat :: ParseTree.Field -> TC LStat
typeCheckLStat field =
 case field of
  ParseTree.StatField surfaceData stat temporality ->
   error "statfield case not implemented"
  ParseTree.HpStatField surfaceData hpStat ->
   case hpStat of
    ParseTree.CurrentHp surfaceData' -> pure $ LHpStat LCurrentHp
    ParseTree.MaxHp surfaceData' -> pure $ LHpStat LMaxHp
    ParseTree.BaseHp surfaceData' -> pure $ LHpStat LBaseHp
  ParseTree.EngagementField surfaceData ->
   error "engagementfield case not implemented"




{-


from parsetree


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

data Field = StatField Lexer.SurfaceData Stat Temporality
           | HpStatField Lexer.SurfaceData HpStat
           | EngagementField Lexer.SurfaceData
           deriving Show

-}
 
typeCheckRStatSelf :: ParseTree.Field -> TC LStat
typeCheckRStatSelf = typeCheckLStat

typeCheckRStatVar :: ParseTree.Field -> TC RStat
typeCheckRStatVar field =
 case field of
  ParseTree.StatField surfaceData stat temporality -> error "statField case not implemented"
  ParseTree.HpStatField surfaceData hpStat -> error "hpstatfield case not implemented"
  ParseTree.EngagementField surfaceData -> error "engagementfield case not implemented"



{-NONE OF THESE CURRENTLY ACCOUNT FOR CARDS NOT BEING ON THE FIELD. E.G., WE SHOULD NOT TARGET THE MODIFIED STATS OF CARDS IN SPAWN-}

{-


data Field = StatField Lexer.SurfaceData Stat Temporality
           | HpStatField Lexer.SurfaceData HpStat
           | EngagementField Lexer.SurfaceData
           deriving Show
-}





{-
 -
 - .... Level MaxHp BaseAttack BaseDefense BaseSpeed BaseRange
 -
 -
 - also engagement...
-}



data SkillEffect = SkillEffectAssignment SurfaceData Assignment {-I need more skill effects, of course-}
                 deriving Show
data Assignment = Assignment SurfaceData [LExpr] ParseTree.Mutator RInt
                deriving Show
data LExpr = LThoughtsExpr SurfaceData ParseTree.Side
           | LKnowledgeExpr SurfaceData Knowledge ParseTree.Side
           | LSelfProjection SurfaceData LStat {-should exclude base stats, soul points...-}
           | LVarProjection SurfaceData Variable LStat
           {- Cardinality not implemented... -}


           deriving Show

      
{-                   
data Mutator = Increment SurfaceData
             | Decrement SurfaceData
             | Assign SurfaceData
             deriving Show

-}









getDistance :: String -> String -> Int
getDistance s1 s2 = restrictedDamerauLevenshteinDistance defaultEditCosts s1 s2



getDistances :: String -> [String] -> [Int]
getDistances s1 ss = map (getDistance s1) ss 


getDistancesWithStrings :: String -> [String] -> [(String,Int)]
getDistancesWithStrings s1 ss = zip ss $ map (getDistance s1) ss


getCloseDistancesWithStrings :: String -> [String] -> [(String,Int)]
getCloseDistancesWithStrings s1 ss = filter (\x -> (snd x) <= 2) $ getDistancesWithStrings s1 ss




getDistanceMessages'' :: Maybe String -> String
getDistanceMessages'' x =
 case x of
  Nothing -> ""
  Just s -> s

getDistanceMessages' :: [(String,Int)] -> Maybe String
getDistanceMessages' [] = Nothing
getDistanceMessages' (x1:x2:xs) = Just ((fst x1) ++ " or " ++ ( getDistanceMessages'' $ getDistanceMessages' (x2:xs)))
getDistanceMessages' (x:[]) = Just ( (fst x) ++ "?")


getDistanceMessages :: String -> [String] -> [String]
getDistanceMessages s ss =
 let x = getCloseDistancesWithStrings s ss in
  case getDistanceMessages' x of
   Nothing -> []
   Just y -> [y]



schoolFromKnowledge :: Knowledge -> Schools
schoolFromKnowledge knowledge =
 case knowledge of
  Earth surfaceData -> EarthMono surfaceData
  Fire surfaceData -> FireMono surfaceData
  Water surfaceData -> WaterMono surfaceData
  Air surfaceData -> AirMono surfaceData
  Spirit surfaceData -> SpiritMono surfaceData
  Void surfaceData -> VoidMono surfaceData







errorPrefix :: Int -> Int -> String
errorPrefix line column = "Syntax error on line " ++ (show line) ++ ", column " ++ (show column) ++ ": "

errorPrefix' :: Lexer.SurfaceData -> String
errorPrefix' (Lexer.SurfaceData line column surface) = errorPrefix line column



typeCheckKnowledge :: ParseTree.Knowledge -> TC Knowledge
typeCheckKnowledge knowledge =
 typeCheckSchool surfaceData
 where ParseTree.Knowledge surfaceData = knowledge

typeCheckSchool :: Lexer.SurfaceData -> TC Knowledge
typeCheckSchool (Lexer.SurfaceData line column surface) =
 let surfaceData = (Lexer.SurfaceData line column surface) in
 case surface of
  "earth" -> pure . Earth $ surfaceData
  "fire" -> pure . Fire $ surfaceData
  "water" -> pure . Water $ surfaceData
  "air" -> pure . Air $ surfaceData
  "spirit" -> pure . Spirit $ surfaceData
  "void" -> pure . Void $ surfaceData
  otherwise ->
   let recommendations = getDistanceMessages otherwise ["earth", "fire", "water", "air", "spirit", "void"] in
   let prefix = errorPrefix line column in
   case recommendations of
    [] -> putErr $ prefix ++ otherwise ++ " is not a valid school."
    _ -> putErr $ prefix ++ otherwise ++ " is not a valid school. Did you mean " ++ (concat recommendations)

showKnowledge :: Knowledge -> String
showKnowledge knowledge =
 case knowledge of
  Earth surfaceData -> "earth"
  Fire surfaceData -> "fire"
  Water surfaceData -> "water"
  Air surfaceData -> "air"
  Spirit surfaceData -> "spirit"
  Void surfaceData -> "void"

{-could be made a typeclass-}
getSurface :: Knowledge -> Lexer.SurfaceData
getSurface knowledge =
 case knowledge of
  Earth surfaceData -> surfaceData
  Fire surfaceData -> surfaceData
  Water surfaceData -> surfaceData
  Air surfaceData -> surfaceData
  Spirit surfaceData -> surfaceData
  Void surfaceData -> surfaceData

schoolsFromKnowledge :: Knowledge -> Knowledge -> TC Schools
schoolsFromKnowledge school1 school2 =
 case (school1,school2) of
  (Earth surfaceData1, Fire surfaceData2) -> pure . EarthFire $ error "earthfire not implemented"
  (Earth surfaceData1, Water surfaceData2) -> pure . EarthWater $ error "earthwater not implemented"
  (Earth surfaceData1, Air surfaceData2) -> pure . EarthAir $ error "earthair not implemented"
  (Earth surfaceData1, Spirit surfaceData2) -> pure . EarthSpirit $ error "earthspirit not implemented"
  (Earth surfaceData1, Void surfaceData2) -> pure . EarthVoid $ error "earthvoid not implemented"
  (Fire surfaceData1, Water surfaceData2) -> pure . FireWater $ error "firewater not implemented"
  (Fire surfaceData1, Air surfaceData2) -> pure . FireAir $ error "fireair not implemented"
  (Fire surfaceData1, Spirit surfaceData2) -> pure . FireSpirit $ error "firespirit not implemented"
  (Fire surfaceData1, Void surfaceData2) -> pure . FireVoid $ error "firevoid not implemented"
  (Water surfaceData1, Air surfaceData2) -> pure . WaterAir $ error "waterair not implemented"
  (Water surfaceData1, Spirit surfaceData2) -> pure . WaterSpirit $ error "waterspirit not implemented"
  (Water surfaceData1, Void surfaceData2) -> pure . WaterVoid $ error "watervoid not implemented"
  (Air surfaceData1, Spirit surfaceData2) -> pure . AirSpirit $ error "airspirit not implemented"
  (Air surfaceData1, Void surfaceData2) -> pure . AirVoid $ error "airvoid not implemented"
  (Spirit surfaceData1, Void surfaceData2) -> pure . SpiritVoid $ error "spiritvoid not implemented"



  (k1,k2) ->
   let ((Lexer.SurfaceData line1 column1 surface1),(Lexer.SurfaceData line2 column2 surface2)) = (getSurface k1, getSurface k2) in
   let prefix = errorPrefix line1 column1 in
   if school1 == school2
    then
     putErr $ prefix ++ "Units cannot belong to two identical schools" {-I should keep the line,column numbers along with the type checker AST, so I can output errors better there...-}
    else
     putErr $ prefix ++ ("Invalid pair of schools: " ++ (showKnowledge school1) ++ " " ++ (showKnowledge school2) ++ ". Did you mean " ++ (showKnowledge school2) ++ " " ++ (showKnowledge school1))
  







joinTC :: TC (TC a) -> TC a
joinTC = TC . join . fmap runTC . runTC


{-Need to wrap variable, and not just have String...-}



{-
getSet :: Context -> Variable -> 
-}





{-
 -
 -I HAVE WHERE, BUT I DO NOT HAVE IF STATEMENTS YET IN MY SKILLS BEYOND THIS... I SHOULD.
 - 
 - -}




typeCheckAutomatic :: Context -> ParseTree.Automatic -> TC Automatic
typeCheckAutomatic context automatic  =
 Automatic surfaceData <$> traverse (typeCheckSkillEffect context) skillEffects
                       <*> typeCheckNonautomatic context nonautomatic
 where ParseTree.Automatic surfaceData skillEffects nonautomatic = automatic



{-
extendContext :: [(String, ParseTree.Set)] -> Context -> Context {-hmm... should be able to throw an error if var already bound?-}
extendContext = error "extend context not implemented"
-}


{-need to make sure that I check the new bindings somewhere to make sure that variable names are no more than 1 character long-}
typeCheckNonautomatic :: Context -> ParseTree.Nonautomatic -> TC Nonautomatic
typeCheckNonautomatic context nonautomatic =
 case nonautomatic of
  ParseTree.Nonautomatic surfaceData newBindings condition thenBranch elseBranch nextBranch ->
   Selection surfaceData (map mkJudgment newBindings)
   <$> typeCheckCondition condition
   <*> (joinTC $ typeCheckAutomatic <$> tryExtendContextMultiple context (map mkJudgment newBindings) <*> pure thenBranch)
   <*> (joinTC $ typeCheckAutomatic <$> tryExtendContextMultiple context (map mkJudgment newBindings) <*> pure elseBranch)
   <*> typeCheckAutomatic context nextBranch {-error "nonautomatic not implemented"-}
  ParseTree.TerminatedSkillComponent -> error "terminated skill component not implemented"


{-
Also need to know that variables in the next branch are disjoint from the then and else branches 
-}


{-


Selection SurfaceData [Judgement] RBool Automatic Automatic Automatic
                  deriving Show

Lexer.SurfaceData [(String, Set)] (Maybe Expr) Automatic Automatic Automatic {-variables, where condition-}
                  | TerminatedSkillComponent


-}


{-(concat $ map (checkSkillEffect context) skillEffects) ++ (checkNonautomatic context nonautomatic) -}


{-
checkNonautomatic :: Context -> ParseTree.Nonautomatic -> [String]
checkNonautomatic context (ParseTree.Nonautomatic surfaceData variables condition thenAutomatic otherwiseAutomatic nextAutomatic) = error "checkNonautomatic not implemented"


-}











lExprError' :: String -> String
lExprError' s = s ++ " is not a valid L expression."

lExprError :: Lexer.SurfaceData -> String
lExprError (Lexer.SurfaceData _ _ s) = lExprError' s



{-Somewhere I want a rule that you cannot project onto soul points.....-}

{-currently no warnings if you try to set a value to a number outside its bound (which would set to the bound instead)-}
{-If there's an error here, I will not look any further at subexpressions for further problems.-}


{-currently, am not checking for how a variable is used (certain stats cannot be accessed or set based on where the card is)-}
typeCheckVariable :: Context -> Variable -> TC Variable
typeCheckVariable context var=
 case varIn var context of
  True -> pure var
  False -> TC $ Left $ ["variable not defined"]



buildLExpr :: Context -> ParseTree.Expr -> TC LExpr
buildLExpr context expr =
 case expr of
  ParseTree.ThoughtsExpr surfaceData side -> pure $ LThoughtsExpr surfaceData side
  ParseTree.KnowledgeExpr surfaceData (ParseTree.Knowledge knowledge) side ->
   LKnowledgeExpr surfaceData <$> typeCheckSchool knowledge
                              <*> pure side
  ParseTree.Self surfaceData field -> LSelfProjection surfaceData <$> typeCheckLStat field
  ParseTree.Var surfaceData field variable ->
   LVarProjection surfaceData <$> typeCheckVariable context (Variable surfaceData variable)
                              <*> typeCheckLStat field

{-I need to check the variable somewhere to make sure that the var is length 1? or is that done in the parser?-}

  ParseTree.Sum surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  ParseTree.Difference surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  ParseTree.Product surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  ParseTree.Quotient surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  ParseTree.Mod surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  ParseTree.Always surfaceData -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData {-Should not have surfaceData here, as the user cannot write always (at least...should not be able to..)-}
  ParseTree.GT surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  ParseTree.GEQ surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  ParseTree.LT surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  ParseTree.LEQ surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  ParseTree.EQ surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  ParseTree.And surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  ParseTree.Or surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  ParseTree.Not surfaceData _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData




{-
isInt :: Context -> ParseTree.Expr -> 

isBool....


Maybe when I make LExprs and RExprs... I should just put the type in the datatype..
-}

{-maybe instead of this function I should have two functions...
 - one for LExprs, and one for RExprs...
 - -}




 {-
isValidBinding :: Context -> ParseTree.Expr -> [String]
isValidBinding context expr =
 case expr of
  ParseTree.ThoughtsExpr side -> []
  ParseTree.KnowledgeExpr knowledge side -> []
  ParseTree.Self field -> [] {-extra check elsewhere that soul and spawn skills don't do this.-}
  ParseTree.Var field string -> 
  ParseTree.Sum expr1 expr2 -> (isValidBinding context expr1) ++ (isValidBinding context expr2) {-check for LExpr rejects this..-}
  ParseTree.Difference expr1 expr2 -> (isValidBinding context expr1) ++ (isValidBinding context expr2)
  ParseTree.Product expr1 expr2 -> (isValidBinding context expr1) ++ (isValidBinding context expr2)
  ParseTree.Quotient expr1 expr2 -> (isValidBinding context expr1) ++ (isValidBinding context expr2)
  ParseTree.Mod expr1 expr2 -> (isValidBinding context expr1) ++ (isValidBinding context expr2)
  ParseTree.Always ->
  ParseTree.GT expr1 expr2 -> 
  ParseTree.GEQ expr1 expr2 -> 
  ParseTree.LT expr1 expr2 -> 
  ParseTree.LEQ expr1 expr2 -> 
  ParseTree.EQ expr1 expr2 -> 
  ParseTree.And expr1 expr2 -> 
  ParseTree.Or expr1 expr2 -> 
  ParseTree.Not expr -> 
-}



{-
isValidLExpr checks against things like.. are you assigning to your base stat, etc. Things that are not an LValue.

And/Or maybe I should just convert to LExprs here.
-}







{-
checkSkillEffect :: Context -> ParseTree.SkillEffect -> [String]
checkSkillEffect context skillEffect =
 case skillEffect of
  (ParseTree.Assignment surfaceData exprs mutator rExpr) -> error "checkSkillEffect case ParseTree.Assignment not implemented"
-}





{-
 - THE difference between Judgements in contexts here, and sets in the parser, is for convenience we might want to additionally allow the soul to be an additional area to select, even though
 - we don't want other cards to select that. The reason for that is so that SELF can be bound then to the soul, and we can typecheck references to self.
 -
 - Another possibility is to have a separate "no selfs" clause that soul skills check (this is probably easier).
 -
 - -}


{-Certain effects and conditions are not valid depending on the set. You cannot damage cards in the graveyard, for instance-}

typeCheckSchools :: ParseTree.Schools -> TC Schools
typeCheckSchools (ParseTree.NoSchools surfaceData) = pure $ NoSchools
typeCheckSchools (ParseTree.OneSchool surfaceData s) = schoolFromKnowledge <$> typeCheckSchool surfaceData
typeCheckSchools (ParseTree.TwoSchools surfaceData s1 s2) = joinTC $ schoolsFromKnowledge <$> typeCheckSchool s1 <*> typeCheckSchool s2




getLocationMessage :: Lexer.SurfaceData -> String
getLocationMessage (Lexer.SurfaceData lineNumber columnNumber _) =
 "on line " ++ (show lineNumber) ++ ", column " ++ (show columnNumber) ++ "\n"
getSurfaceSyntax :: Lexer.SurfaceData -> String
getSurfaceSyntax (Lexer.SurfaceData _ _ surfaceSyntax) = surfaceSyntax ++ "\n"


typeCheckNumber :: ParseTree.Expr -> TC RInt {-called typecheckrint below...-}
typeCheckNumber = error "do not use this function"


{-should add isDead to conditions?-}
typeCheckRBool :: Context -> ParseTree.Expr -> TC RBool {-call typeCheckRBool?-}
typeCheckRBool context expr =
 case expr of
  ParseTree.Constant surfaceData value ->
    TC $ Left ["Type mismatch between Boolean (required type) and Integer (type of integer literal) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  ParseTree.ThoughtsExpr surfaceData side ->                             {-Ignoring distinction between Thought and Thoughts for now-}
    TC $ Left ["Type mismatch between Boolean (required type) and Integer (type of thoughts) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  ParseTree.KnowledgeExpr surfaceData knowledge side ->
   TC $ Left ["Type mismatch between Boolean (required type) and Integer (type of knowledge) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  ParseTree.Self surfaceData field ->
   TC $ Left ["Type mismatch between Boolean (required type) and Integer (type of projection from self) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  ParseTree.Var surfaceData field var ->
   TC $ Left ["Type mismatch between Boolean (required type) and Integer (type of projection from variable " ++ var ++ ") in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  ParseTree.Sum surfaceData expr1 expr2 ->
    TC $ Left ["Type mismatch between Boolean (requied type) and Integer (result type of (+) operator) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]    
  ParseTree.Difference surfaceData expr1 expr2 ->
    TC $ Left ["Type mismatch between Boolean (required type) and Integer (result type of (-) operator) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  ParseTree.Product surfaceData expr1 expr2 ->
   TC $ Left ["Type mismatch between Boolean (required type) and Integer (result type of (*) operator) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  ParseTree.Quotient surfaceData expr1 expr2 ->
   TC $ Left ["Type mismatch between Boolean (required type) and Integer (result type of (/) operator) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  ParseTree.Mod surfaceData expr1 expr2 ->                                      {-Have to make sure this implements modulus, not remainder...-}
   TC $ Left ["Type mismatch between Boolean (required type) and Integer (result type of modulus operator) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  ParseTree.Always surfaceData -> error "always not implemented, and should not exist" {-add more booleans later.....    again... nullable.... ALWAYS SHOULD BE REMOVED UNTIL CODE GEN PHASE.-}
  ParseTree.GT surfaceData expr1 expr2 ->
   RGT surfaceData <$> typeCheckRInt context expr1
                   <*> typeCheckRInt context expr2
  ParseTree.GEQ surfaceData expr1 expr2 ->
   RGEQ surfaceData <$> typeCheckRInt context expr1
                    <*> typeCheckRInt context expr2
  ParseTree.LT surfaceData expr1 expr2 ->
   RLT surfaceData <$> typeCheckRInt context expr1
                   <*> typeCheckRInt context expr2
  ParseTree.LEQ surfaceData expr1 expr2 ->
   RLEQ surfaceData <$> typeCheckRInt context expr1
                    <*> typeCheckRInt context expr2
  ParseTree.EQ surfaceData expr1 expr2 ->
   REQ surfaceData <$> typeCheckRInt context expr1
                   <*> typeCheckRInt context expr2
  ParseTree.And surfaceData expr1 expr2 ->
   RAnd surfaceData <$> typeCheckRBool context expr1
                    <*> typeCheckRBool context expr2
  ParseTree.Or surfaceData expr1 expr2 ->
   ROr surfaceData <$> typeCheckRBool context expr1
                   <*> typeCheckRBool context expr2
  ParseTree.Not surfaceData expr ->
   RNot surfaceData <$> typeCheckRBool context expr

typeCheckSkill :: ParseTree.Skill -> TC Skill
typeCheckSkill (ParseTree.AutomaticSkill surfaceData cost condition automatic) =
 trace "typecheckskill not implemented"
 Skill surfaceData <$> typeCheckCost cost
                   <*> typeCheckCondition condition
                   <*> typeCheckAutomatic EmptyContext automatic




typeCheckCost :: Maybe ParseTree.Expr -> TC (Maybe RInt)
typeCheckCost Nothing = pure Nothing
typeCheckCost (Just expr) = Just <$> typeCheckRInt EmptyContext expr


typeCheckCondition :: Maybe ParseTree.Expr -> TC (Maybe RBool)
typeCheckCondition Nothing = pure Nothing
typeCheckCondition (Just expr) = Just <$> typeCheckRBool EmptyContext expr


{-
checkAutomatic :: Context -> ParseTree.Automatic -> TC Automatic
data Skill = Skill SurfaceData RInt RBool Automatic {-Currently no check against this cost being negative. Also doesn't have to be a constant (design decision)-}
 

-}


typeCheckStart :: Maybe ParseTree.Start -> TC (Maybe Start)
typeCheckStart Nothing = pure Nothing
typeCheckStart (Just (ParseTree.Start surfaceData skill)) =
 trace "typeCheckStart not implemented" $
 Just <$> Start surfaceData
      <$> typeCheckSkill skill

typeCheckEnd :: Maybe ParseTree.End -> TC (Maybe End)
typeCheckEnd Nothing = pure Nothing
typeCheckEnd (Just (ParseTree.End surfaceData skill)) =
 trace "typecheckEnd not implemented" $
 Just <$> End surfaceData
      <$> typeCheckSkill skill


typeCheckCounter :: Maybe ParseTree.Counter -> TC (Maybe Counter)
typeCheckCounter Nothing = pure Nothing
typeCheckCounter (Just (ParseTree.Counter surfaceData skill)) =
 trace "typecheckCounter not implemented" $
 Just <$> Counter surfaceData
      <$> typeCheckSkill skill

typeCheckSpawnUnit :: Maybe ParseTree.Spawn -> TC (Maybe SpawnUnit)
typeCheckSpawnUnit Nothing = pure Nothing
typeCheckSpawnUnit (Just (ParseTree.Spawn surfaceData skill)) =
 trace "typecheckSpawnUnit not implemented" $
 Just <$> SpawnUnit surfaceData
      <$> typeCheckSkill skill

typeCheckDeath :: Maybe ParseTree.Death -> TC (Maybe Death)
typeCheckDeath Nothing = pure Nothing
typeCheckDeath (Just (ParseTree.Death surfaceData skill)) =
 trace "typecheckDeath not implemented" $
 Just <$> Death surfaceData
      <$> typeCheckSkill skill

typeCheckAuto :: Maybe ParseTree.Auto -> TC (Maybe Auto)
typeCheckAuto Nothing = pure Nothing
typeCheckAuto (Just (ParseTree.Auto surfaceData skill)) =
 trace "typecheckauto not implemented" $
 Just <$> Auto surfaceData
      <$> typeCheckSkill skill

typeCheckAction :: ParseTree.Action -> TC Action
typeCheckAction (ParseTree.Action surfaceData skill) =
 trace "typeCheckAction not implemented" $
 Action surfaceData <$> typeCheckSkill skill

typeCheckActions :: [ParseTree.Action] -> TC [Action]
typeCheckActions = traverse typeCheckAction





{-


data Skill = Skill SurfaceData (Maybe RInt) (Maybe RBool) Automatic {-Currently no check against this cost being negative. Also doesn't have to be a constant (design decision)-}
           deriving Show


-}


noSelfReferencesCost :: Maybe RInt -> TC (Maybe RInt)
noSelfReferencesCost = undefined

noSelfReferencesCondition :: Maybe RBool -> TC (Maybe RBool)
noSelfReferencesCondition = undefined

noSelfReferencesAutomatic :: Automatic -> TC Automatic
noSelfReferencesAutomatic = undefined

noSelfReferencesNonautomatic :: Nonautomatic -> TC Nonautomatic
noSelfReferencesNonautomatic = undefined


noSelfReferencesSkill :: Skill -> TC Skill
noSelfReferencesSkill skill = error "noSelfReferencesSkill not implemented"

noSelfReferences :: (Skill -> a) -> Skill -> TC a
noSelfReferences f skill =
 f <$> noSelfReferencesSkill skill

typeCheckSoul :: ParseTree.Soul -> TC Soul
typeCheckSoul (ParseTree.Soul surfaceData skill) =
 trace "typeCheckSoul not implemented" $
 joinTC $ noSelfReferences (Soul surfaceData)
 <$> typeCheckSkill skill


{-level, etc, should be an arbitrary string in parsing... but a number after type checking..-}
typeCheckBaseLevel :: Lexer.SurfaceData -> TC BaseLevel
typeCheckBaseLevel (Lexer.SurfaceData row column surface) = 
 BaseLevel (Lexer.SurfaceData row column surface) <$> (typeCheckInt surface "Base level" 1 9)
 

typeCheckBaseHp :: Lexer.SurfaceData -> TC BaseHp
typeCheckBaseHp (Lexer.SurfaceData row column surface) =
 BaseHp (Lexer.SurfaceData row column surface) <$> (typeCheckInt surface "Base hp" 1 1000) 


typeCheckBaseAttack :: SurfaceData -> TC BaseAttack
typeCheckBaseAttack (Lexer.SurfaceData row column surface) =
 BaseAttack (Lexer.SurfaceData row column surface) <$> (typeCheckInt surface "Base attack" 0 1000)

typeCheckInt :: String -> String -> Int -> Int -> TC Int
typeCheckInt s name lowerBound upperBound =
 case (readMaybe s :: Maybe Int) of
  Nothing -> TC $ Left [name ++ " must be an integer"]
  Just i ->
   if i < lowerBound then putErr $ name ++ " must be at least " ++ (show lowerBound)
   else if i > upperBound then putErr $ name ++ " cannot exceed " ++ (show upperBound)
   else pure i

typeCheckConstant :: Lexer.SurfaceData -> String -> TC Int
typeCheckConstant (Lexer.SurfaceData row column surfaceSyntax) s = typeCheckInt s ("at position " ++ (show row) ++ "," ++ (show column) ++ ", " ++ "constant expression" ++ " " ++ surfaceSyntax) minInt maxInt

typeCheckLInt :: Context -> ParseTree.Expr -> TC LExpr {-and apparently these are all ints...-}
typeCheckLInt context expr =
 case expr of
  ParseTree.Constant surfaceData value ->
   TC $ Left ["cannot assign to constant"]
  ParseTree.ThoughtsExpr surfaceData side -> pure $ LThoughtsExpr surfaceData side
  ParseTree.KnowledgeExpr surfaceData knowledge side ->
   LKnowledgeExpr surfaceData
   <$> typeCheckKnowledge knowledge
   <*> pure side
  ParseTree.Self surfaceData field ->
   LSelfProjection surfaceData <$> typeCheckRStatSelf field
  ParseTree.Var surfaceData field variable ->
   LVarProjection surfaceData <$> typeCheckVariable context (Variable surfaceData variable)
                              <*> typeCheckRStatSelf field
  ParseTree.Sum surfaceData expr1 expr2 ->
   TC $ Left ["cannot assign to sum result"]
  ParseTree.Difference surfaceData expr1 expr2 ->
   TC $ Left ["cannot assign to difference result"]
  ParseTree.Product surfaceData expr1 expr2 ->
   TC $ Left ["cannot assign to product result"]
  ParseTree.Quotient surfaceData expr1 expr2 ->
   TC $ Left ["cannot assign to quotient result"]
  ParseTree.Mod surfaceData expr1 expr2 ->
   TC $ Left ["cannot assign to modulus result"]
  ParseTree.Always surfaceData -> error "always should not exist, much less as an integer"
  ParseTree.GT surfaceData expr1 expr2 ->
   TC $ Left ["cannot assign to inequality check"]
  ParseTree.GEQ surfaceData expr1 expr2 ->
    TC $ Left ["cannot assign to inequality check"]
  ParseTree.LT surfaceData expr1 expr2 ->
   TC $ Left ["cannot assign to inequality check"]
  ParseTree.LEQ surfaceData expr1 expr2 ->
   TC $ Left ["cannot assign to inequality check"]
  ParseTree.EQ surfaceData expr1 expr2 ->
   TC $ Left ["cannot assign to equality check"]
  ParseTree.And surfaceData expr1 expr2 ->
   TC $ Left ["cannot assign to and result"]
  ParseTree.Or surfaceData expr1 expr2 ->
   TC $ Left ["cannot assign to or result"]
  ParseTree.Not surfaceData expr ->
   TC $ Left ["cannot assign to not result"]



typeCheckRInt :: Context -> ParseTree.Expr -> TC RInt
typeCheckRInt context expr = {-error "typeCheckRInt not implemented"-}
 case expr of
  ParseTree.Constant surfaceData value -> RConstant surfaceData <$> typeCheckConstant surfaceData value
  ParseTree.ThoughtsExpr surfaceData side -> pure $ RThoughts surfaceData side
  ParseTree.KnowledgeExpr surfaceData knowledge side -> 
   RKnowledge surfaceData
   <$> typeCheckKnowledge knowledge
   <*> pure side
  ParseTree.Self surfaceData field ->
   RSelfProjection surfaceData <$> typeCheckRStatSelf field
  ParseTree.Var surfaceData field variable ->
   RVarProjection surfaceData <$> typeCheckRStatVar field
                              <*> typeCheckVariable context (Variable surfaceData variable)
  ParseTree.Sum surfaceData expr1 expr2 ->
   RSum surfaceData <$> typeCheckRInt context expr1
                    <*> typeCheckRInt context expr2
  ParseTree.Difference surfaceData expr1 expr2 ->
   RDifference surfaceData <$> typeCheckRInt context expr1
                           <*> typeCheckRInt context expr2
  ParseTree.Product surfaceData expr1 expr2 ->
   RProduct surfaceData <$> typeCheckRInt context expr1
                        <*> typeCheckRInt context expr2
  ParseTree.Quotient surfaceData expr1 expr2 ->
   RQuotient surfaceData <$> typeCheckRInt context expr1
                         <*> typeCheckRInt context expr2
  ParseTree.Mod surfaceData expr1 expr2 ->
   RMod surfaceData <$> typeCheckRInt context expr1
                    <*> typeCheckRInt context expr2

{-PARSE TREE ALWAYS STILL EXISTS!??!?!?!?!?!?!?!??!?!?!?!?!?-}

  ParseTree.Always surfaceData -> error "always should not exist, much less as an integer"
  ParseTree.GT surfaceData expr1 expr2 ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "(>)"]
  ParseTree.GEQ surfaceData expr1 expr2 ->
    TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "(>=)"]
  ParseTree.LT surfaceData expr1 expr2 ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "(<)"]
  ParseTree.LEQ surfaceData expr1 expr2 ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "(<=)"]
  ParseTree.EQ surfaceData expr1 expr2 ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "(=)"]
  ParseTree.And surfaceData expr1 expr2 ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "and"]
  ParseTree.Or surfaceData expr1 expr2 ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "or"]
  ParseTree.Not surfaceData expr ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "not"]

typeMismatchMessage :: Lexer.SurfaceData -> String -> String -> String -> String
typeMismatchMessage surfaceData requiredType resultType operator =
 "Type mismatch between " ++
 requiredType ++
 " (required type) and " ++
 resultType ++
 " (result type of " ++
 operator ++
 ") in subexpression:\n" ++
 (getSurfaceSyntax surfaceData) ++
 (getLocationMessage surfaceData)



{-Again... need to pass row and column to typecheckint... can actually pass entire surface syntax to it...-}
typeCheckBaseDefense :: SurfaceData -> TC BaseDefense
typeCheckBaseDefense (Lexer.SurfaceData row column surface) =
 BaseDefense (Lexer.SurfaceData row column surface) <$> (typeCheckInt surface "Base defense" 0 1000)
typeCheckBaseSpeed :: SurfaceData -> TC BaseSpeed
typeCheckBaseSpeed (Lexer.SurfaceData row column surface) =
 BaseSpeed (Lexer.SurfaceData row column surface) <$> (typeCheckInt surface "Base speed" 1 5)
typeCheckBaseRange :: SurfaceData -> TC BaseRange
typeCheckBaseRange (Lexer.SurfaceData row column surface) =
 BaseRange (Lexer.SurfaceData row column surface) <$> (typeCheckInt surface "Base range" 1 5)
typeCheckBaseSoulPoints :: SurfaceData -> TC BaseSoulPoints
typeCheckBaseSoulPoints (Lexer.SurfaceData row column surface) =
 BaseSoulPoints (Lexer.SurfaceData row column surface) <$> (typeCheckInt surface "Base soul points" 1 2)
typeCheckStats :: ParseTree.Stats -> TC Stats
typeCheckStats (ParseTree.Stats surfaceData schools level hp attack defense speed range soulPoints) =
 Stats surfaceData <$> typeCheckSchools schools
                   <*> typeCheckBaseLevel level
                   <*> typeCheckBaseHp hp
                   <*> typeCheckBaseAttack attack
                   <*> typeCheckBaseDefense defense
                   <*> typeCheckBaseSpeed speed
                   <*> typeCheckBaseRange range
                   <*> typeCheckBaseSoulPoints soulPoints
 




typeCheckSpawnSpell :: ParseTree.Skill -> TC Skill
typeCheckSpawnSpell skill = trace "typeCheckSpawnSpell not implemented" $ typeCheckSkill skill

typeCheckSpell :: ParseTree.Spell -> TC Spell
typeCheckSpell (ParseTree.Spell surfaceData name (ParseTree.Knowledge surfaceDataSchool) level skill) =
 Spell surfaceData name <$> typeCheckSchool surfaceDataSchool
                        <*> typeCheckBaseLevel level
                        <*> typeCheckSpawnSpell skill


typeCheckUnit :: ParseTree.Unit -> TC Unit
typeCheckUnit (ParseTree.Unit surfaceData name stats start end counter spawn death auto actions soul) =
 Unit surfaceData name <$> typeCheckStats stats
                       <*> typeCheckStart start 
                       <*> typeCheckEnd end
                       <*> typeCheckCounter counter
                       <*> typeCheckSpawnUnit spawn
                       <*> typeCheckDeath death
                       <*> typeCheckAuto auto
                       <*> typeCheckActions actions 
                       <*> typeCheckSoul soul


typeCheckUnits :: [ParseTree.Unit] -> TC [Unit]
typeCheckUnits = traverse typeCheckUnit

typeCheckSpells :: [ParseTree.Spell] -> TC [Spell]
typeCheckSpells = traverse typeCheckSpell


assumeFailure :: Either [String] a -> [String]
assumeFailure (Left s) = s
assumeFailure (Right _) = []


typeCheck :: ParseTree.File -> TC File
typeCheck (ParseTree.File surfaceData units spells) =
 File surfaceData <$> (typeCheckUnits units)
      <*> (typeCheckSpells spells)


 {-cabal install edit-distance-}





{-


main = do
 x <- getContents
 case Lexer.runAlex x calc of
  Right y -> error $ show $ (prettyPrint $ map extractSurface (getTokens x)) {-show y-}
  Left y -> error $ show {-x-} y


-}

      
       
        
newtype TC a = TC {runTC :: Either [String] a} deriving Functor

getErr (TC (Right _)) = []
getErr (TC (Left x)) = x

putErr :: String -> TC a
putErr = TC . Left . pure

putErrs :: [String] -> TC a
putErrs = TC . Left



instance Applicative TC where
 pure = TC . Right
 (TC (Right f)) <*> (TC (Right x)) = TC . Right . f $ x
 m <*> n = TC . Left $ (getErr m) <> (getErr n)





