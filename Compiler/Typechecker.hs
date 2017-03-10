{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Typechecker where    
import qualified Lexer
import qualified Parser
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






data File = File [Unit] [Spell]
          deriving Show
data Unit = Unit String Stats (Maybe Start) (Maybe End) (Maybe Counter) (Maybe Spawn) (Maybe Death) (Maybe Auto) [Action] Soul
          deriving Show
data Spell = Spell String Knowledge BaseLevel Skill {- name, school, level, skill -}
           deriving Show
data Skill = Skill RNat RBool Automatic
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


data RNat = RNat Int
          deriving Show
data RBool = RBool Bool
           deriving Show
data Knowledge = Earth
               | Fire
               | Water
               | Air
               | Spirit
               | Void
               deriving (Eq,Show)

data Automatic = Automatic [SkillEffect] Nonautomatic
               deriving Show
data Nonautomatic = Selection Variables RBool Automatic Automatic
                  deriving Show
data Stats = Stats Schools BaseLevel BaseHp BaseAttack BaseDefense BaseSpeed BaseRange BaseSoulPoints
           deriving Show
data Schools = NoSchools
             | EarthMono
             | FireMono
             | WaterMono
             | AirMono
             | SpiritMono
             | VoidMono
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
data BaseLevel = BaseLevel Int
               deriving Show
data BaseHp = BaseHp Int
            deriving Show
data BaseAttack = BaseAttack Int
                deriving Show
data BaseDefense = BaseDefense Int
                 deriving Show
data BaseSpeed = BaseSpeed Int
               deriving Show
data BaseRange = BaseRange Int
               deriving Show
data BaseSoulPoints = BaseSoulPoints Int
                    deriving Show



data Variables = Variables [(String,Set)]
               deriving Show
data Set = SimpleSet Side RelativeSet
         | UnionSet Set Set
         deriving Show
data Side = Friendly
          | Enemy
          deriving Show
data RelativeSet = Field
                 | Hand
                 | Graveyard
                 | Banished
                 | SpawnLocation
                 deriving Show


data SkillEffect = SkillEffectAssignment Assignment {-I need more skill effects, of course-}
                 deriving Show
data Assignment = Assignment [LExpr] Mutator RExpr
                deriving Show
data LExpr = LExpr {-None yet-}
           deriving Show
data RExpr = RExpr {-None yet-}
           deriving Show
data Mutator = Increment
             | Decrement
             | Assign
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
  Earth -> EarthMono
  Fire -> FireMono
  Water -> WaterMono
  Air -> AirMono
  Spirit -> SpiritMono
  Void -> VoidMono



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


data Variable = Variable String {-String of length 1-}
data Context = EmptyContext
             | ExtendContext Context (Variable, Set)

{-
getSet :: Context -> Variable -> 
-}

checkAutomatic :: Context -> Parser.Automatic -> [String]
checkAutomatic = undefined
checkNonautomatic :: Context -> Parser.Nonautomatic -> [String]
checkNonautomatic = undefined
checkSkillEffect :: Context -> Parser.SkillEffect -> [String]
checkSkillEffect = undefined

checkSet :: Parser.Set -> [String]
checkSet = undefined


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



typeCheckSoul :: Parser.Soul -> TC Soul
typeCheckSoul soul = undefined



{-level, etc, should be an arbitrary string in parsing... but a number after type checking..-}
typeCheckBaseLevel :: String -> TC BaseLevel
typeCheckBaseLevel x = 
 BaseLevel <$> (typeCheckInt x "Base level" 1 9)
 

typeCheckBaseHp :: String -> TC BaseHp
typeCheckBaseHp x =
 BaseHp <$> (typeCheckInt x "Base hp" 1 1000) 


typeCheckBaseAttack :: String -> TC BaseAttack
typeCheckBaseAttack x =
 BaseAttack <$> (typeCheckInt x "Base attack" 0 1000)

typeCheckInt :: String -> String -> Int -> Int -> TC Int
typeCheckInt s name lowerBound upperBound =
 case (readMaybe s :: Maybe Int) of
  Nothing -> TC $ Left [name ++ " must be an int"]
  Just i ->
   if i < lowerBound then putErr $ name ++ " must be at least " ++ (show lowerBound)
   else if i > upperBound then putErr $ name ++ " cannot exceed " ++ (show upperBound)
   else pure i

typeCheckBaseDefense :: String -> TC BaseDefense
typeCheckBaseDefense x =
 BaseDefense <$> (typeCheckInt x "Base defense" 0 1000)
typeCheckBaseSpeed :: String -> TC BaseSpeed
typeCheckBaseSpeed x =
 BaseSpeed <$> (typeCheckInt x "Base speed" 1 5)
typeCheckBaseRange :: String -> TC BaseRange
typeCheckBaseRange x =
 BaseRange <$> (typeCheckInt x "Base range" 1 5)
typeCheckBaseSoulPoints :: String -> TC BaseSoulPoints
typeCheckBaseSoulPoints x =
 BaseSoulPoints <$> (typeCheckInt x "Base soul points" 1 2)
typeCheckStats :: Parser.Stats -> TC Stats
typeCheckStats (Parser.Stats schools level hp attack defense speed range soulPoints) =
 Stats <$> typeCheckSchools schools
       <*> typeCheckBaseLevel level
       <*> typeCheckBaseHp hp
       <*> typeCheckBaseAttack attack
       <*> typeCheckBaseDefense defense
       <*> typeCheckBaseSpeed speed
       <*> typeCheckBaseRange range
       <*> typeCheckBaseSoulPoints soulPoints
 
typeCheckSpawnSpell :: Parser.Skill -> TC Skill
typeCheckSpawnSpell _ = undefined

typeCheckSpell :: Parser.Spell -> TC Spell
typeCheckSpell (Parser.Spell name (Parser.Knowledge school) level skill) =
 Spell <$> (TC . Right $ name) <*> (typeCheckSchool $ school) <*> (typeCheckBaseLevel $ level) <*> (typeCheckSpawnSpell $ skill)


typeCheckUnit :: Parser.Unit -> TC Unit
typeCheckUnit (Parser.Unit name stats start end counter spawn death auto actions soul) =
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





