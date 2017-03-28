{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Typechecker where    
import qualified Lexer
import qualified Parser
import Lexer(SurfaceData)
import Text.Read
import Text.EditDistance
import Data.Monoid
import Control.Monad(join)
import Debug.Trace 
  
   

maxInt :: Int
maxInt = 1000


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
data Skill = Skill SurfaceData RInt RBool Automatic {-Currently no check against this cost being negative. Also doesn't have to be a constant (design decision)-}
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

data RInt = RThoughts SurfaceData Parser.Side
          | RKnowledge SurfaceData Parser.Knowledge Parser.Side
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





data Automatic = Automatic SurfaceData [SkillEffect] Nonautomatic
               deriving Show
data Nonautomatic = Selection SurfaceData [Judgement] RBool Automatic Automatic
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
data Judgement = Judgement SurfaceData (Variable,Set)
               deriving Show

data Variable = Variable SurfaceData String {-String of length 1-}
              deriving Show
data Context = EmptyContext
             | ExtendContext Context Judgement
             deriving Show

data Set = SimpleSet SurfaceData Parser.Side RelativeSet
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

data LStat = LStat {-unimplemented-}
           deriving Show
data RStat = RStat {-unimplemented. Like LStat but allows reference to base-}
            deriving Show
typeCheckLStat :: Parser.Field -> TC LStat
typeCheckLStat = error "lstat not implemented"
 


{-
 -
 - .... Level MaxHp BaseAttack BaseDefense BaseSpeed BaseRange
 -
 -
 - also engagement...
-}



data SkillEffect = SkillEffectAssignment SurfaceData Assignment {-I need more skill effects, of course-}
                 deriving Show
data Assignment = Assignment SurfaceData [Judgement] Mutator RInt
                deriving Show
data LExpr = LThoughtsExpr SurfaceData Parser.Side
           | LKnowledgeExpr SurfaceData Knowledge Parser.Side
           | LSelfProjection SurfaceData LStat {-should exclude base stats, soul points...-}
           | LVarProjection SurfaceData Variable LStat
           {- Cardinality not implemented... -}

{-
   Parser.Self surfaceData field -> undefined
      Parser.Var surfaceData field string -> undefined
-}

           deriving Show

      
                   
data Mutator = Increment SurfaceData
             | Decrement SurfaceData
             | Assign SurfaceData
             deriving Show











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
  (Earth surfaceData1, Fire surfaceData2) -> pure . EarthFire $ undefined
  (Earth surfaceData1, Water surfaceData2) -> pure . EarthWater $ undefined
  (Earth surfaceData1, Air surfaceData2) -> pure . EarthAir $ undefined
  (Earth surfaceData1, Spirit surfaceData2) -> pure . EarthSpirit $ undefined
  (Earth surfaceData1, Void surfaceData2) -> pure . EarthVoid $ undefined
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




checkAutomatic :: Context -> Parser.Automatic -> TC Automatic
checkAutomatic context (Parser.Automatic surfaceData skillEffects nonautomatic) = error "checkAutomatic not implemented"



{-(concat $ map (checkSkillEffect context) skillEffects) ++ (checkNonautomatic context nonautomatic) -}

checkNonautomatic :: Context -> Parser.Nonautomatic -> [String]
checkNonautomatic context (Parser.Nonautomatic surfaceData variables condition thenAutomatic otherwiseAutomatic nextAutomatic) = error "checkNonautomatic not implemented"














lExprError' :: String -> String
lExprError' s = s ++ " is not a valid L expression."

lExprError :: Lexer.SurfaceData -> String
lExprError (Lexer.SurfaceData _ _ s) = lExprError' s



{-Somewhere I want a rule that you cannot project onto soul points.....-}

{-currently no warnings if you try to set a value to a number outside its bound (which would set to the bound instead)-}
{-If there's an error here, I will not look any further at subexpressions for further problems.-}



typeCheckVariable :: Context -> Variable -> TC Variable
typeCheckVariable = undefined



buildLExpr :: Context -> Parser.Expr -> TC LExpr
buildLExpr context expr =
 case expr of
  Parser.ThoughtsExpr surfaceData side -> pure $ LThoughtsExpr surfaceData side
  Parser.KnowledgeExpr surfaceData (Parser.Knowledge knowledge) side -> LKnowledgeExpr surfaceData <$> typeCheckSchool knowledge <*> pure side
  Parser.Self surfaceData field -> LSelfProjection surfaceData <$> typeCheckLStat field
  Parser.Var surfaceData field variable -> LVarProjection surfaceData <$> typeCheckVariable context (Variable surfaceData variable) <*> typeCheckLStat field {-I need to check the variable somewhere to make sure that the var is length 1? or is that done in the parser?-}
  Parser.Sum surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  Parser.Difference surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  Parser.Product surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  Parser.Quotient surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  Parser.Mod surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  Parser.Always surfaceData -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData {-Should not have surfaceData here, as the user cannot write always (at least...should not be able to..)-}
  Parser.GT surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  Parser.GEQ surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  Parser.LT surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  Parser.LEQ surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  Parser.EQ surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  Parser.And surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  Parser.Or surfaceData _ _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData
  Parser.Not surfaceData _ -> putErr $ (errorPrefix' surfaceData) ++ lExprError surfaceData




{-
isInt :: Context -> Parser.Expr -> 

isBool....


Maybe when I make LExprs and RExprs... I should just put the type in the datatype..
-}

{-maybe instead of this function I should have two functions...
 - one for LExprs, and one for RExprs...
 - -}




 {-
isValidBinding :: Context -> Parser.Expr -> [String]
isValidBinding context expr =
 case expr of
  Parser.ThoughtsExpr side -> []
  Parser.KnowledgeExpr knowledge side -> []
  Parser.Self field -> [] {-extra check elsewhere that soul and spawn skills don't do this.-}
  Parser.Var field string -> undefined
  Parser.Sum expr1 expr2 -> (isValidBinding context expr1) ++ (isValidBinding context expr2) {-check for LExpr rejects this..-}
  Parser.Difference expr1 expr2 -> (isValidBinding context expr1) ++ (isValidBinding context expr2)
  Parser.Product expr1 expr2 -> (isValidBinding context expr1) ++ (isValidBinding context expr2)
  Parser.Quotient expr1 expr2 -> (isValidBinding context expr1) ++ (isValidBinding context expr2)
  Parser.Mod expr1 expr2 -> (isValidBinding context expr1) ++ (isValidBinding context expr2)
  Parser.Always -> undefined
  Parser.GT expr1 expr2 -> undefined
  Parser.GEQ expr1 expr2 -> undefined
  Parser.LT expr1 expr2 -> undefined
  Parser.LEQ expr1 expr2 -> undefined
  Parser.EQ expr1 expr2 -> undefined
  Parser.And expr1 expr2 -> undefined
  Parser.Or expr1 expr2 -> undefined
  Parser.Not expr -> undefined
-}



{-
isValidLExpr checks against things like.. are you assigning to your base stat, etc. Things that are not an LValue.

And/Or maybe I should just convert to LExprs here.
-}



checkSkillEffect :: Context -> Parser.SkillEffect -> [String]
checkSkillEffect context skillEffect =
 case skillEffect of
  (Parser.Assignment surfaceData exprs mutator rExpr) -> error "checkSkillEffect case Parser.Assignment not implemented"

{-
 - THE difference between Judgements in contexts here, and sets in the parser, is for convenience we might want to additionally allow the soul to be an additional area to select, even though
 - we don't want other cards to select that. The reason for that is so that SELF can be bound then to the soul, and we can typecheck references to self.
 -
 - Another possibility is to have a separate "no selfs" clause that soul skills check (this is probably easier).
 -
 - -}



{-
checkSet :: Parser.Set -> [String]
checkSet = undefined
-}




{-Certain effects and conditions are not valid depending on the set. You cannot damage cards in the graveyard, for instance-}



{-
checkStart :: Parser.Skill -> [String]
checkStart = undefined
checkEnd :: Parser.Skill -> [String]
checkEnd = undefined
checkCounter :: Parser.Skill -> [String]
checkCounter = undefined
checkDeath :: Parser.Skill -> [String]
checkDeath = undefined
checkAuto :: Parser.Skill -> [String]
checkAuto = undefined
checkAction :: Parser.Skill -> [String]
checkAction = undefined
checkSoul :: Parser.Skill -> [String]
checkSoul = undefined
-}
{-Should use a phantom type for some of these?-}


typeCheckSchools :: Parser.Schools -> TC Schools
typeCheckSchools (Parser.NoSchools surfaceData) = pure $ NoSchools
typeCheckSchools (Parser.OneSchool surfaceData s) = schoolFromKnowledge <$> typeCheckSchool surfaceData
typeCheckSchools (Parser.TwoSchools surfaceData s1 s2) = joinTC $ schoolsFromKnowledge <$> typeCheckSchool s1 <*> typeCheckSchool s2




getLocationMessage :: Lexer.SurfaceData -> String
getLocationMessage (Lexer.SurfaceData lineNumber columnNumber _) =
 "on line " ++ (show lineNumber) ++ ", column " ++ (show columnNumber) ++ "\n"
getSurfaceSyntax :: Lexer.SurfaceData -> String
getSurfaceSyntax (Lexer.SurfaceData _ _ surfaceSyntax) = surfaceSyntax ++ "\n"


typeCheckNumber :: Parser.Expr -> TC RInt {-called typecheckrint below...-}
typeCheckNumber = error "do not use this function"


{-should add isDead to conditions?-}
typeCheckCondition :: Parser.Expr -> TC RBool {-call typeCheckRBool?-}
typeCheckCondition expr =
 case expr of
  Parser.Constant surfaceData value ->
    TC $ Left ["Type mismatch between Boolean (expected type) and Int (type of int literal) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  Parser.ThoughtsExpr surfaceData side ->                             {-Ignoring distinction between Thought and Thoughts for now-}
    TC $ Left ["Type mismatch between Boolean (expected type) and Int (type of thoughts) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  Parser.KnowledgeExpr surfaceData knowledge side ->
   TC $ Left ["Type mismatch between Boolean (expected type) and Int (type of knowledge) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  Parser.Self surfaceData field ->
   TC $ Left ["Type mismatch between Boolean (expected type) and Int (type of projection from self) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  Parser.Var surfaceData field var -> undefined
  Parser.Sum surfaceData expr1 expr2 ->
    TC $ Left ["Type mismatch between Boolean (expected type) and Int (result type of (+) operator) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]    
  Parser.Difference surfaceData expr1 expr2 ->
    TC $ Left ["Type mismatch between Boolean (expected type) and Int (result type of (-) operator) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  Parser.Product surfaceData expr1 expr2 ->
   TC $ Left ["Type mismatch between Boolean (expected type) and Int (result type of (*) operator) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  Parser.Quotient surfaceData expr1 expr2 ->
   TC $ Left ["Type mismatch between Boolean (expected type) and Int (result type of (/) operator) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  Parser.Mod surfaceData expr1 expr2 ->                                      {-Have to make sure this implements modulus, not remainder...-}
   TC $ Left ["Type mismatch between Boolean (expected type) and Int (result type of modulus operator) in subexpression:\n" ++ (getSurfaceSyntax surfaceData) ++ (getLocationMessage surfaceData)]
  Parser.Always surfaceData -> undefined {-add more booleans later.....    again... nullable.... ALWAYS SHOULD BE REMOVED UNTIL CODE GEN PHASE.-}
  Parser.GT surfaceData expr1 expr2 ->
   RGT surfaceData <$> typeCheckRInt expr1
                   <*> typeCheckRInt expr2
  Parser.GEQ surfaceData expr1 expr2 ->
   RGEQ surfaceData <$> typeCheckRInt expr1
                    <*> typeCheckRInt expr2
  Parser.LT surfaceData expr1 expr2 ->
   RLT surfaceData <$> typeCheckRInt expr1
                   <*> typeCheckRInt expr2
  Parser.LEQ surfaceData expr1 expr2 ->
   RLEQ surfaceData <$> typeCheckRInt expr1
                    <*> typeCheckRInt expr2
  Parser.EQ surfaceData expr1 expr2 ->
   REQ surfaceData <$> typeCheckRInt expr1
                   <*> typeCheckRInt expr2
  Parser.And surfaceData expr1 expr2 ->
   RAnd surfaceData <$> typeCheckCondition expr1
                    <*> typeCheckCondition expr2
  Parser.Or surfaceData expr1 expr2 ->
   ROr surfaceData <$> typeCheckCondition expr1
                   <*> typeCheckCondition expr2
  Parser.Not surfaceData expr ->
   RNot surfaceData <$> typeCheckCondition expr

typeCheckSkill :: Parser.Skill -> TC Skill
typeCheckSkill (Parser.AutomaticSkill surfaceData cost condition automatic) = undefined



typeCheckStart :: Maybe Parser.Start -> TC (Maybe Start)
typeCheckStart Nothing = pure Nothing
typeCheckStart (Just (Parser.Start surfaceData skill)) =
 trace "typeCheckStart not implemented" $
 Just <$> Start surfaceData
      <$> typeCheckSkill skill

typeCheckEnd :: Maybe Parser.End -> TC (Maybe End)
typeCheckEnd Nothing = pure Nothing
typeCheckEnd (Just (Parser.End surfaceData skill)) =
 trace "typecheckEnd not implemented" $
 Just <$> End surfaceData
      <$> typeCheckSkill skill


typeCheckCounter :: Maybe Parser.Counter -> TC (Maybe Counter)
typeCheckCounter Nothing = pure Nothing
typeCheckCounter (Just (Parser.Counter surfaceData skill)) =
 trace "typecheckCounter not implemented" $
 Just <$> Counter surfaceData
      <$> typeCheckSkill skill

typeCheckSpawnUnit :: Maybe Parser.Spawn -> TC (Maybe SpawnUnit)
typeCheckSpawnUnit Nothing = pure Nothing
typeCheckSpawnUnit (Just (Parser.Spawn surfaceData skill)) =
 trace "typecheckSpawnUnit not implemented" $
 Just <$> SpawnUnit surfaceData
      <$> typeCheckSkill skill

typeCheckDeath :: Maybe Parser.Death -> TC (Maybe Death)
typeCheckDeath Nothing = pure Nothing
typeCheckDeath (Just (Parser.Death surfaceData skill)) =
 trace "typecheckDeath not implemented" $
 Just <$> Death surfaceData
      <$> typeCheckSkill skill

typeCheckAuto :: Maybe Parser.Auto -> TC (Maybe Auto)
typeCheckAuto Nothing = pure Nothing
typeCheckAuto (Just (Parser.Auto surfaceData skill)) =
 trace "typecheckauto not implemented" $
 Just <$> Auto surfaceData
      <$> typeCheckSkill skill

typeCheckAction :: Parser.Action -> TC Action
typeCheckAction (Parser.Action surfaceData skill) =
 trace "typeCheckAction not implemented" $
 Action surfaceData <$> typeCheckSkill skill

typeCheckActions :: [Parser.Action] -> TC [Action]
typeCheckActions = traverse typeCheckAction



noSelfReferences :: Parser.Skill -> [String]
noSelfReferences skill = trace "noSelfReferences not implemented" []

typeCheckSoul :: Parser.Soul -> TC Soul
typeCheckSoul (Parser.Soul surfaceData skill) = trace "typeCheckSoul not implemented" $ Soul surfaceData <$> typeCheckSkill skill



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
  Nothing -> TC $ Left [name ++ " must be an int"]
  Just i ->
   if i < lowerBound then putErr $ name ++ " must be at least " ++ (show lowerBound)
   else if i > upperBound then putErr $ name ++ " cannot exceed " ++ (show upperBound)
   else pure i


typeCheckRInt :: Parser.Expr -> TC RInt
typeCheckRInt = error "typeCheckRInt not implemented"



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
typeCheckStats :: Parser.Stats -> TC Stats
typeCheckStats (Parser.Stats surfaceData schools level hp attack defense speed range soulPoints) =
 Stats surfaceData <$> typeCheckSchools schools
                   <*> typeCheckBaseLevel level
                   <*> typeCheckBaseHp hp
                   <*> typeCheckBaseAttack attack
                   <*> typeCheckBaseDefense defense
                   <*> typeCheckBaseSpeed speed
                   <*> typeCheckBaseRange range
                   <*> typeCheckBaseSoulPoints soulPoints
 




typeCheckSpawnSpell :: Parser.Skill -> TC Skill
typeCheckSpawnSpell skill = trace "typeCheckSpawnSpell not implemented" $ typeCheckSkill skill

typeCheckSpell :: Parser.Spell -> TC Spell
typeCheckSpell (Parser.Spell surfaceData name (Parser.Knowledge surfaceDataSchool) level skill) =
 Spell surfaceData <$> (TC . Right $ name) <*> (typeCheckSchool surfaceDataSchool) <*> (typeCheckBaseLevel $ level) <*> (typeCheckSpawnSpell skill)


typeCheckUnit :: Parser.Unit -> TC Unit
typeCheckUnit (Parser.Unit surfaceData name stats start end counter spawn death auto actions soul) =
 Unit surfaceData <$> pure "name"  {- Unit name <$> all the other stuff -}
                  <*> typeCheckStats stats
                  <*> typeCheckStart start 
                  <*> typeCheckEnd end
                  <*> typeCheckCounter counter
                  <*> typeCheckSpawnUnit spawn
                  <*> typeCheckDeath death
                  <*> typeCheckAuto auto
                  <*> typeCheckActions actions 
                  <*> typeCheckSoul soul


typeCheckUnits :: [Parser.Unit] -> TC [Unit]
typeCheckUnits = traverse typeCheckUnit

typeCheckSpells :: [Parser.Spell] -> TC [Spell]
typeCheckSpells = traverse typeCheckSpell


assumeFailure :: Either [String] a -> [String]
assumeFailure (Left s) = s
assumeFailure (Right _) = []


typeCheck :: Parser.File -> TC File
typeCheck (Parser.File surfaceData units spells) =
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





