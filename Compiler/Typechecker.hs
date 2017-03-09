{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Typechecker where    
import qualified Lexer
import qualified Parser
import Text.Read
import Text.EditDistance
import Data.Monoid

 
  
   

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
data Spell = Spell String Knowledge String Skill {- name, school, level, skill -}
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
               deriving Show

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



typeCheckSchool :: String -> [String]
typeCheckSchool "earth" = []
typeCheckSchool "fire" = []
typeCheckSchool "water" = []
typeCheckSchool "air" = []
typeCheckSchool "spirit" = []
typeCheckSchool "void" = []
typeCheckSchool s =
 let x = getDistanceMessages s ["earth", "fire", "water", "air", "spirit", "void"] in
 case x of
  [] -> [s ++ " is not a valid school."]
  _ -> [s ++ " is not a valid school. Did you mean " ++ (concat x) ]


typeCheckSchools :: Parser.Schools -> Either [String] Schools
typeCheckSchools _ = undefined                 



{-
typeCheckSkill :: Skill -> [String]
typeCheckSkill _ = undefined
-}



typeCheckStart :: Maybe Parser.Start -> Either [String] (Maybe Start)
typeCheckStart _ = undefined

typeCheckEnd :: Maybe Parser.End -> Either [String] (Maybe End)
typeCheckEnd _ = undefined

typeCheckCounter :: Maybe Parser.Counter -> Either [String] (Maybe Counter)
typeCheckCounter _ = undefined

typeCheckSpawnUnit :: Maybe Parser.Spawn -> Either [String] (Maybe Spawn)
typeCheckSpawnUnit _ = undefined

typeCheckDeath :: Maybe Parser.Death -> Either [String] (Maybe Death)
typeCheckDeath _ = undefined

typeCheckAuto :: Maybe Parser.Auto -> Either [String] (Maybe Auto)
typeCheckAuto _ = undefined

typeCheckAction :: Parser.Action -> Either [String] Action
typeCheckAction _ = undefined

typeCheckActions :: [Parser.Action] -> Either [String] [Action]
typeCheckActions _ = undefined



typeCheckSoul :: Parser.Soul -> Either [String] Soul
typeCheckSoul _ = undefined



{-level, etc, should be an arbitrary string in parsing... but a number after type checking..-}
typeCheckBaseLevel :: String -> Either [String] BaseLevel
typeCheckBaseLevel s = 
 case (readMaybe s :: Maybe Int) of
  Nothing -> Left ["Base level must be an int."]
  Just i ->
   if i < 1 then Left ["Base level must be at least 1."]
   else if i > 9 then Left ["Base level must be at most 9."]
   else Right $ BaseLevel i




typeCheckBaseHp :: String -> Either [String] BaseHp
typeCheckBaseHp x =
 case typeCheckInt x "Base hp" 1 1000 of
  Left s -> Left s
  Right i -> Right $ BaseHp i




typeCheckBaseAttack :: String -> Either [String] BaseAttack
typeCheckBaseAttack x =
 case typeCheckInt x "Base attack" 0 1000 of
  Left s -> Left s
  Right i -> Right $ BaseAttack i

typeCheckInt :: String -> String -> Int -> Int -> Either [String] Int
typeCheckInt s name lowerBound upperBound =
 case (readMaybe s :: Maybe Int) of
  Nothing -> Left [name ++ " must be an int"]
  Just i ->
   if i < lowerBound then Left [name ++ " must be at least " ++ (show lowerBound)]
   else if i > upperBound then Left [name ++ " cannot exceed " ++ (show upperBound)]
   else Right i


{- This above can be refactored to remove a lot of redundancy...-}


typeCheckBaseDefense :: String -> Either [String] BaseDefense
typeCheckBaseDefense x =
 case typeCheckInt x "Base defense" 0 1000 of
  Left s -> Left s
  Right i -> Right $ BaseDefense i
typeCheckBaseSpeed :: String -> Either [String] BaseSpeed
typeCheckBaseSpeed x =
 case typeCheckInt x "Base speed" 1 5 of
  Left s -> Left s
  Right i -> Right $ BaseSpeed i
typeCheckBaseRange :: String -> Either [String] BaseRange
typeCheckBaseRange x =
 case typeCheckInt x "Base range" 1 5 of
  Left s -> Left s
  Right i -> Right $ BaseRange i
typeCheckBaseSoulPoints :: String -> Either [String] BaseSoulPoints
typeCheckBaseSoulPoints x =
 case typeCheckInt x "Base soul points" 2 3 of
  Left s -> Left s
  Right i -> Right $ BaseSoulPoints i
typeCheckStats :: Parser.Stats -> Either [String] Stats
typeCheckStats (Parser.Stats schools level hp attack defense speed range soulPoints) = undefined {-Stats <$> undefined <$> undefined <$> undefined <$> undefined <$> undefined <$> undefined <$> undefined-}
 
 {-case (typeCheckSchools schools, typeCheckBaseLevel level, typeCheckBaseHp hp, typeCheckBaseAttack attack, typeCheckBaseDefense defense, typeCheckBaseSpeed speed, typeCheckBaseRange range, typeCheckBaseSoulPoints soulPoints) of
  (Right correctSchools, Right correctLevel, Right correctHp, Right correctAttack, Right correctDefense, Right correctSpeed, Right correctRange, Right correctSoulPoints) ->
   undefined
  (failedSchools, failedLevel, failedHp, failedAttack, failedDefense, failedSpeed, failedRange, failedSoulPoints) ->
   Left $ (assumeFailure failedSchools) ++ (assumeFailure failedHp) ++ (assumeFailure failedAttack) ++ (assumeFailure failedDefense) ++ (assumeFailure failedSpeed) ++ (assumeFailure failedRange) ++ (assumeFailure failedSoulPoints)
-}


typeCheckSpawnSpell :: Parser.Skill -> [String]
typeCheckSpawnSpell _ = undefined

typeCheckSpell :: Parser.Spell -> Either [String] Spell
typeCheckSpell (Parser.Spell name (Parser.Knowledge school) level skill) = undefined
{- (typeCheckSchool school) ++
 (typeCheckBaseLevel level) ++
 (typeCheckSpawnSpell skill)
-}



collect :: [Either [String] a] -> Either [String] [a]
collect [] = Right []
collect ((Left s):xs) =
 case collect xs of
  Right _ -> Left s
  Left s2 -> Left (s ++ s2)
collect ((Right x):xs) = collect xs




typeCheckUnit :: Parser.Unit -> Either [String] Unit
typeCheckUnit (Parser.Unit name stats start end counter spawn death auto actions soul) =
 Unit <$> (Right "name") <*> (typeCheckStats stats) <*> (typeCheckStart start) <*> (typeCheckEnd end) <*> (typeCheckCounter counter) <*> (typeCheckSpawnUnit spawn) <*> (typeCheckDeath death) <*> (typeCheckAuto auto) <*> (typeCheckActions actions) <*> (typeCheckSoul soul)


typeCheckUnits :: [Parser.Unit] -> Either [String] [Unit]
typeCheckUnits = undefined

typeCheckSpells :: [Parser.Spell] -> Either [String] [Spell]
typeCheckSpells = undefined


assumeFailure :: Either [String] a -> [String]
assumeFailure (Left s) = s
assumeFailure (Right _) = []


typeCheck :: Parser.File -> Either [String] File
typeCheck (Parser.File units spells) =
 File <$> (typeCheckUnits units) <*> (typeCheckSpells spells)



 {-
 
 case (collect $ map typeCheckUnit units, collect $ map typeCheckSpell spells) of
  (Right correctUnits, Right correctSpells) -> Right $ File correctUnits correctSpells
  (failedUnits, failedSpells) -> Left $ (assumeFailure failedUnits) ++ (assumeFailure failedSpells)

-}


{-(concat $ map typeCheckUnit units) ++ (concat $ map typeCheckSpell spells)-}

{-
data Attempt success = Attempt success [String]
instance Applicative => Monad Attempt where
-}































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

instance Applicative TC where
 pure = TC . Right
 (TC (Right f)) <*> (TC (Right x)) = TC . Right . f $ x
 m <*> n = TC . Left $ (getErr m) <> (getErr n)





