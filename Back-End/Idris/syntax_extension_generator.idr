import Data.Vect



stats : String
stats = "syntax [unit_name] \"<-\" [schools] lvl \":\" [level] life \":\" [hp] atk \":\" [attack] def \":\" [defense] spe \":\" [speed] rng \":\" [range] sp \":\" [soulPoints]\n"

skills : Vect 5 String
skills = ["start","end","counter","spawn","death"] -- does not include auto, soul, or action(s)


f : Bool -> String -> String
f True s = "(Just " ++ s ++ ") "
f False _ = "Nothing "

data PossibleActions = NoActions | OneAction | ManyActions

stringifyAction : PossibleActions -> String
stringifyAction NoActions = "[]"
stringifyAction OneAction = "[actionSkill]"
stringifyAction ManyActions = "actionSkills"


suffix : Vect 5 Bool -> Bool -> PossibleActions -> String
suffix v True act = (foldr (++) "" (zipWith f v (map (++ "Skill") skills))) ++ "(Just autoSkill) " ++ (stringifyAction act) ++ " soulSkill"
suffix v False act = (foldr (++) "" (zipWith f v (map (++ "Skill") skills))) ++ "Nothing " ++ (stringifyAction act) ++ " soulSkill"



g : String -> String
g s = s ++ " \":\" [" ++ s ++ "Skill] "

h : Bool -> String -> String
h True s = g s
h False _ = ""


baz : Vect 5 Bool -> Bool -> PossibleActions -> String
baz v True act = (foldr (++) "" (zipWith h v skills)) ++ " \"auto\" \":\" [autoSkill] " ++ (stringifyAction act)
baz v False act = (foldr (++) "" (zipWith h v skills)) ++ (stringifyAction act)

{-have to zip the prefix with the suffixes, to make sure they correspond-}

lb : List Bool
lb = [True,False]
allPossible5 : List (Vect 5 Bool) 
allPossible5 = [[x1,x2,x3,x4,x5] | x1 <- lb, x2 <- lb, x3 <- lb, x4 <- lb, x5 <- lb]


{- this can be improved... -}
allPossibleSuffix : List String
allPossibleSuffix = (map (\x => suffix x True NoActions) allPossible5) ++ (map (\x => suffix x True OneAction) allPossible5) ++ (map (\x => suffix x True ManyActions) allPossible5) ++ (map (\x => suffix x False NoActions) allPossible5) ++ (map (\x => suffix x False OneAction) allPossible5) ++ (map (\x => suffix x False ManyActions) allPossible5)


qq : Bool -> String -> (String,String)
qq True s = (s ++ " \":\" [" ++ s ++ "Skill]","(Just " ++ s ++ ") ")
qq False s = ("","")



theSuffixPart : Vect 5 Bool -> Bool -> PossibleActions -> String
theSuffixPart v True act = ((foldr (++) "" (zipWith f v (map (++ "Skill") skills))) ++ "(Just autoSkill) " ++ (stringifyAction act) ++ " soulSkill")
theSuffixPart v False act = ((foldr (++) "" (zipWith f v (map (++ "Skill") skills))) ++ (stringifyAction act) ++ " soulSkill")

thePrefixPart : Vect 5 Bool -> Bool -> PossibleActions -> String
thePrefixPart v b act = (foldr (++) "" (zipWith h v skills)) {-haven't dealt with b or act yet... stkill working-}



prefixAndSuffix : Vect 5 Bool -> Bool -> PossibleActions -> (String,String)
prefixAndSuffix v b act = (thePrefixPart v b act,theSuffixPart v b act)




allPossiblePrefixAndSuffix : List (String,String)
allPossiblePrefixAndSuffix = map (\x => prefixAndSuffix x False NoActions) allPossible5

allStrings : List String
allStrings = ?hole


printAll : List String -> IO ()
printAll [] = pure ()
printAll (x::xs) = putStrLn x >>= (\_ => printAll xs)

main : IO ()
main = putStrLn (show (allPossiblePrefixAndSuffix))



{-

main = printAll allPossibleSuffix


-}

{-
main = putStrLn (show allPossibleSuffix)
-}
{-
main = putStrLn (suffix [True,False,False,True,True] True NoActions)
-}










