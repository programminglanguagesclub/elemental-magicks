{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Typechecker where    
import qualified Lexer
import qualified Parser
import Parser(SurfaceData)
import Text.Read
import Text.EditDistance
import Data.Monoid
import Control.Monad(join)
 
  
   

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
data Unit = Unit SurfaceData String Stats (Maybe Start) (Maybe End) (Maybe Counter) (Maybe Spawn) (Maybe Death) (Maybe Auto) [Action] Soul
          deriving Show
data Spell = Spell SurfaceData String Knowledge BaseLevel Skill {- name, school, level, skill -}
           deriving Show
data Skill = Skill SurfaceData RNat RBool Automatic
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


data RNat = RNat SurfaceData Int
          deriving Show
data RBool = RBool SurfaceData Bool
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

data Set = SimpleSet SurfaceData Side RelativeSet
         | UnionSet SurfaceData Set Set
         deriving Show
data Side = Friendly SurfaceData
          | Enemy SurfaceData
          deriving Show
data RelativeSet = Field SurfaceData
                 | Hand SurfaceData
                 | Graveyard SurfaceData
                 | Banished SurfaceData
                 | SpawnLocation SurfaceData
                 deriving Show


data SkillEffect = SkillEffectAssignment SurfaceData Assignment {-I need more skill effects, of course-}
                 deriving Show
data Assignment = Assignment SurfaceData [Judgement] Mutator RExpr
                deriving Show
data LExpr = LExpr SurfaceData {-None yet-}
           deriving Show
data RExpr = RExpr SurfaceData {-None yet-}
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



typeCheckSchool :: String -> TC Knowledge
typeCheckSchool "earth" = pure Earth
typeCheckSchool "fire" = pure Fire
typeCheckSchool "water" = pure Water
typeCheckSchool "air" = pure Air
typeCheckSchool "spirit" = pure Spirit
typeCheckSchool "void" = pure Void
typeCheckSchool s =
 let x = getDistanceMessages s ["earth", "fire", "water", "air", "spirit", "void"] in
 case x of
  [] -> putErr $ s ++ " is not a valid school."
  _ -> putErr $ s ++ " is not a valid school. Did you mean " ++ (concat x)



showKnowledge :: Knowledge -> String
showKnowledge knowledge =
 case knowledge of
  Earth -> "earth"
  Fire -> "fire"
  Water -> "water"
  Air -> "air"
  Spirit -> "spirit"
  Void -> "void"


schoolsFromKnowledge :: Knowledge -> Knowledge -> TC Schools
schoolsFromKnowledge school1 school2 =
 case (school1,school2) of
  (Earth,Fire) -> pure EarthFire
  (Earth,Water) -> pure EarthWater
  (Earth,Air) -> pure EarthAir
  (Earth,Spirit) -> pure EarthSpirit
  (Earth,Void) -> pure EarthVoid
  _ ->
   if school1 == school2
    then
     putErr "Units cannot belong to two identical schools" {-I should keep the line,column numbers along with the type checker AST, so I can output errors better there...-}
    else
     putErr ("Invalid pair of schools: " ++ (showKnowledge school1) ++ " " ++ (showKnowledge school2) ++ ". Did you mean " ++ (showKnowledge school2) ++ " " ++ (showKnowledge school1))
  







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
checkAutomatic context (Parser.Automatic skillEffects nonautomatic) = undefined {-(concat $ map (checkSkillEffect context) skillEffects) ++ (checkNonautomatic context nonautomatic) -}

checkNonautomatic :: Context -> Parser.Nonautomatic -> [String]
checkNonautomatic context (Parser.Nonautomatic variables condition thenAutomatic otherwiseAutomatic nextAutomatic) = undefined













{-If there's an error here, I will not look any further at subexpressions for further problems.-}
buildLExpr :: Context -> Parser.Expr -> TC LExpr
buildLExpr context expr =
 case expr of
  Parser.ThoughtsExpr side -> pure undefined
  Parser.KnowledgeExpr knowledge side -> pure undefined
  Parser.Self field -> undefined
  Parser.Var field string -> undefined
  Parser.Sum _ _ -> undefined {-need appropriate error message. Similarly on other cases.-}
  Parser.Difference _ _ -> undefined
  Parser.Product _ _ -> putErr undefined
  Parser.Quotient _ _ -> putErr undefined
  Parser.Mod _ _ -> putErr undefined
  Parser.Always -> putErr undefined
  Parser.GT _ _ -> putErr undefined
  Parser.GEQ _ _ -> putErr undefined
  Parser.LT _ _ -> putErr undefined
  Parser.LEQ _ _ -> putErr undefined
  Parser.EQ _ _ -> putErr undefined
  Parser.And _ _ -> putErr undefined
  Parser.Or _ _ -> putErr undefined
  Parser.Not _ -> putErr undefined


{-
isInt :: Context -> Parser.Expr -> 

isBool....


Maybe when I make LExprs and RExprs... I should just put the type in the datatype..
-}

{-maybe instead of this function I should have two functions...
 - one for LExprs, and one for RExprs...
 - -}
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




{-
isValidLExpr checks against things like.. are you assigning to your base stat, etc. Things that are not an LValue.

And/Or maybe I should just convert to LExprs here.
-}



checkSkillEffect :: Context -> Parser.SkillEffect -> [String]
checkSkillEffect context skillEffect =
 case skillEffect of
  (Parser.Assignment exprs mutator rExpr) -> undefined

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

{-Should use a phantom type for some of these?-}


typeCheckSchools :: Parser.Schools -> TC Schools
typeCheckSchools Parser.NoSchools = pure NoSchools
typeCheckSchools (Parser.OneSchool s) = schoolFromKnowledge <$> typeCheckSchool s
typeCheckSchools (Parser.TwoSchools s1 s2) = joinTC $ schoolsFromKnowledge <$> typeCheckSchool s1 <*> typeCheckSchool s2






typeCheckCondition :: Parser.Expr -> TC RBool
typeCheckCondition = undefined


typeCheckSkill :: Parser.Skill -> TC Skill
typeCheckSkill (Parser.AutomaticSkill cost condition automatic) = undefined



typeCheckStart :: Maybe Parser.Start -> TC (Maybe Start)
typeCheckStart Nothing = undefined
typeCheckStart (Just start) = undefined

typeCheckEnd :: Maybe Parser.End -> TC (Maybe End)
typeCheckEnd Nothing = undefined
typeCheckEnd (Just end) = undefined


typeCheckCounter :: Maybe Parser.Counter -> TC (Maybe Counter)
typeCheckCounter Nothing = undefined
typeCheckCounter (Just counter) = undefined

typeCheckSpawnUnit :: Maybe Parser.Spawn -> TC (Maybe Spawn)
typeCheckSpawnUnit Nothing = undefined
typeCheckSpawnUnit (Just spawn) = undefined

typeCheckDeath :: Maybe Parser.Death -> TC (Maybe Death)
typeCheckDeath Nothing = undefined
typeCheckDeath (Just death) = undefined

typeCheckAuto :: Maybe Parser.Auto -> TC (Maybe Auto)
typeCheckAuto Nothing = undefined
typeCheckAuto (Just auto) = undefined

typeCheckAction :: Parser.Action -> TC Action
typeCheckAction action = undefined

typeCheckActions :: [Parser.Action] -> TC [Action]
typeCheckActions = traverse typeCheckAction



noSelfReferences :: Parser.Skill -> [String]
noSelfReferences skill = undefined

typeCheckSoul :: Parser.Soul -> TC Soul
typeCheckSoul soul = undefined



{-level, etc, should be an arbitrary string in parsing... but a number after type checking..-}
typeCheckBaseLevel :: SurfaceData -> String -> TC BaseLevel
typeCheckBaseLevel sd x = 
 BaseLevel <$> (typeCheckInt x "Base level" 1 9)
 

typeCheckBaseHp :: SurfaceData -> String -> TC BaseHp
typeCheckBaseHp sd x =
 BaseHp <$> (typeCheckInt x "Base hp" 1 1000) 


typeCheckBaseAttack :: SurfaceData -> String -> TC BaseAttack
typeCheckBaseAttack sd x =
 BaseAttack <$> (typeCheckInt x "Base attack" 0 1000)

typeCheckInt :: String -> String -> Int -> Int -> TC Int
typeCheckInt s name lowerBound upperBound =
 case (readMaybe s :: Maybe Int) of
  Nothing -> TC $ Left [name ++ " must be an int"]
  Just i ->
   if i < lowerBound then putErr $ name ++ " must be at least " ++ (show lowerBound)
   else if i > upperBound then putErr $ name ++ " cannot exceed " ++ (show upperBound)
   else pure i

typeCheckBaseDefense :: SurfaceData -> String -> TC BaseDefense
typeCheckBaseDefense sd x =
 BaseDefense <$> (typeCheckInt x "Base defense" 0 1000)
typeCheckBaseSpeed :: SurfaceData -> String -> TC BaseSpeed
typeCheckBaseSpeed sd x =
 BaseSpeed <$> (typeCheckInt x "Base speed" 1 5)
typeCheckBaseRange :: SurfaceData -> String -> TC BaseRange
typeCheckBaseRange sd x =
 BaseRange <$> (typeCheckInt x "Base range" 1 5)
typeCheckBaseSoulPoints :: SurfaceData -> String -> TC BaseSoulPoints
typeCheckBaseSoulPoints sd x =
 BaseSoulPoints <$> (typeCheckInt x "Base soul points" 1 2)
typeCheckStats :: Parser.Stats -> TC Stats
typeCheckStats (Parser.Stats surfaceData schools level hp attack defense speed range soulPoints) =
 Stats <$> typeCheckSchools surfaceData schools
       <*> typeCheckBaseLevel surfaceData level
       <*> typeCheckBaseHp surfaceData hp
       <*> typeCheckBaseAttack surfaceData attack
       <*> typeCheckBaseDefense surfaceData defense
       <*> typeCheckBaseSpeed speed
       <*> typeCheckBaseRange range
       <*> typeCheckBaseSoulPoints soulPoints
 

WORKING HERE



typeCheckSpawnSpell :: Parser.Skill -> TC Skill
typeCheckSpawnSpell _ = undefined

typeCheckSpell :: Parser.Spell -> TC Spell
typeCheckSpell (Parser.Spell surfaceData name (Parser.Knowledge school) level skill) =
 Spell <$> (TC . Right $ name) <*> (typeCheckSchool $ school) <*> (typeCheckBaseLevel $ level) <*> (typeCheckSpawnSpell $ skill)


typeCheckUnit :: Parser.Unit -> TC Unit
typeCheckUnit (Parser.Unit surfaceData name stats start end counter spawn death auto actions soul) =
 Unit <$> pure "name"  {- Unit name <$> all the other stuff -}
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
typeCheck (Parser.File units spells) =
 File <$> (typeCheckUnits units)
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





