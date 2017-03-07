module Typechecker where    
import qualified Lexer
import qualified Parser
import Text.Read
import Text.EditDistance

 
  
   

maxInt :: Int
maxInt = 1000


{-
levenshteinDistance

defaultEditCosts :: EditCosts

levenshteinDistance :: EditCosts -> String -> String -> Int

Find the Levenshtein edit distance between two strings. That is to say, the number of deletion, insertion and substitution operations that are required to make the two strings equal. Note that this algorithm therefore does not make use of the transpositionCost field of the costs. See also: http://en.wikipedia.org/wiki/Levenshtein_distance.

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


typeCheckSchools :: Parser.Schools -> [String]
typeCheckSchools _ = undefined                 



{-
typeCheckSkill :: Skill -> [String]
typeCheckSkill _ = undefined
-}



typeCheckStart :: Maybe Parser.Start -> [String]
typeCheckStart _ = undefined

typeCheckEnd :: Maybe Parser.End -> [String]
typeCheckEnd _ = undefined

typeCheckCounter :: Maybe Parser.Counter -> [String]
typeCheckCounter _ = undefined

typeCheckSpawnUnit :: Maybe Parser.Spawn -> [String]
typeCheckSpawnUnit _ = undefined

typeCheckDeath :: Maybe Parser.Death -> [String]
typeCheckDeath _ = undefined

typeCheckAuto :: Maybe Parser.Auto -> [String]
typeCheckAuto _ = undefined

typeCheckAction :: Parser.Action -> [String]
typeCheckAction _ = undefined

typeCheckSoul :: Parser.Soul -> [String]
typeCheckSoul _ = undefined



{-level, etc, should be an arbitrary string in parsing... but a number after type checking..-}
typeCheckBaseLevel :: String -> [String]
typeCheckBaseLevel s = 
 case (readMaybe s :: Maybe Int) of
  Nothing -> ["Base level must be an int."]
  Just i ->
   if i < 1 then ["Base level must be at least 1."]
   else if i > 9 then ["Base level must be at most 9."]
   else []



typeCheckBaseHp :: String -> [String]
typeCheckBaseHp s =
 case (readMaybe s :: Maybe Int) of
  Nothing -> ["Base hp must be an int"]
  Just i ->
   if i < 1 then ["Base hp must be at least 1"]
   else if i > maxInt then ["Base hp cannot exceed maximum stat value of " ++ (show maxInt)]
   else []

typeCheckAttack :: String -> [String]
typeCheckAttack s =
 case (readMaybe s :: Maybe Int) of
  Nothing -> ["Base attack must be an int"]
  Just i ->
   if i < 0 then ["Base attack must be at least 0"]
   else if i > maxInt then ["Base hp cannot exceed maximum stat value of " ++ (show maxInt)]
   else []

{- This above can be refactored to remove a lot of redundancy...-}


typeCheckDefense :: String -> [String]
typeCheckDefense _ = undefined
typeCheckBaseSpeed :: String -> [String]
typeCheckBaseSpeed _ = undefined
typeCheckBaseRange :: String -> [String]
typeCheckBaseRange _ = undefined
typeCheckBaseSoulPoints :: String -> [String]
typeCheckBaseSoulPoints _ = undefined

typeCheckStats :: Parser.Stats -> [String]
typeCheckStats (Parser.Stats schools level hp attack defense speed range soulPoints) =
 (typeCheckSchools schools) ++
 (typeCheckBaseLevel level) ++
 (typeCheckBaseHp hp) ++
 (typeCheckAttack attack) ++
 (typeCheckDefense defense) ++
 (typeCheckBaseSpeed speed) ++
 (typeCheckBaseRange range) ++
 (typeCheckBaseSoulPoints soulPoints)

typeCheckSpawnSpell :: Parser.Skill -> [String]
typeCheckSpawnSpell _ = undefined

typeCheckSpell :: Parser.Spell -> [String]
typeCheckSpell (Parser.Spell name (Parser.Knowledge school) level skill) =
 (typeCheckSchool school) ++
 (typeCheckBaseLevel level) ++
 (typeCheckSpawnSpell skill)

typeCheckUnit :: Parser.Unit -> [String]
typeCheckUnit (Parser.Unit name stats start end counter spawn death auto actions soul) =
 (typeCheckStats stats) ++
 (typeCheckStart start) ++
 (typeCheckEnd end) ++
 (typeCheckCounter counter) ++
 (typeCheckSpawnUnit spawn) ++
 (typeCheckDeath death) ++
 (typeCheckAuto auto) ++
 (concat $ map typeCheckAction actions) ++
 (typeCheckSoul soul)

typeCheck :: Parser.File -> [String]
typeCheck (Parser.File units spells) = (concat $ map typeCheckUnit units) ++ (concat $ map typeCheckSpell spells)


 {-cabal install edit-distance-}






















{-


main = do
 x <- getContents
 case Lexer.runAlex x calc of
  Right y -> error $ show $ (prettyPrint $ map extractSurface (getTokens x)) {-show y-}
  Left y -> error $ show {-x-} y


-}

      
       
        
         
          
