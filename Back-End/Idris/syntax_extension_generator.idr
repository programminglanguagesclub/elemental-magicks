import Data.Vect



stats : String
stats = "syntax [unit_name] \"<-\" [schools] lvl \":\" [level] life \":\" [hp] atk \":\" [attack] def \":\" [defense] spe \":\" [speed] rng \":\" [range] sp \":\" [soulPoints]\n"

skills : Vect 5 String
skills = ["start","end","counter","spawn","death"] -- does not include auto, soul, or action(s)


f : Bool -> String -> String
f True s = "(Just " ++ s ++ ") "
f False _ = "Nothing "

data PossibleActions = NoActions
                     | OneAction
                     | TwoActions
                     | ThreeActions
                     | FourActions

{- I should probably also bake into the types that there are at most 4 action skills. -}


{- note that this generator is a reasonable place (for now at least) to include a requirement on the maximum total number of skills (5 seems good) -}
{- can also use a Nat for this.. -}
stringifyAction : PossibleActions -> String
stringifyAction NoActions = "[]"
stringifyAction OneAction = "[actionSkill]"
stringifyAction TwoActions = "[actionSkill1, actionSkill2]"
stringifyAction ThreeActions = "[actionSkill1, actionSkill2, actionSkill3]"
stringifyAction FourActions = "[actionSkill1, actionSkill2, actionSkill3, actionSkill4]"


{- can use repeat.. -}
stringifyActionPrefix : PossibleActions -> String
stringifyActionPrefix NoActions = ""
stringifyActionPrefix OneAction = "action \":\" [actionSkill] "
stringifyActionPrefix TwoActions = "action \":\" [actionSkill1] " ++ "action \":\" [actionSkill2] "
stringifyActionPrefix ThreeActions = "action \":\" [actionSkill1] " ++ "action \":\" [actionSkill2] " ++ "action \":\" [actionSkill3] "
stringifyActionPrefix FourActions = "action \":\" [actionSkill1] " ++ "action \":\" [actionSkill2] " ++ "action \":\" [actionSkill3] " ++ "action \":\" [actionSkill4] "



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





{- STILL WORKING HERE!! -}


{- this can be improved... -}
allPossibleSuffix : List String
allPossibleSuffix = (map () ([NoActions, OneAction, TwoActions, ThreeActions, FourActions])) ++ (map () ([NoActions, OneAction, TwoActions, ThreeActions, FourActions]))
allPossibleSuffix = (map (\x => suffix x True NoActions) allPossible5) ++ (map (\x => suffix x True OneAction) allPossible5) ++ (map (\x => suffix x True ManyActions) allPossible5) ++ (map (\x => suffix x False NoActions) allPossible5) ++ (map (\x => suffix x False OneAction) allPossible5) ++ (map (\x => suffix x False ManyActions) allPossible5)


qq : Bool -> String -> (String,String)
qq True s = (s ++ " \":\" [" ++ s ++ "Skill]","(Just " ++ s ++ ") ")
qq False s = ("","")






theSuffixPart : Vect 5 Bool -> Bool -> PossibleActions -> String
theSuffixPart v True act = ((foldr (++) "" (zipWith f v (map (++ "Skill") skills))) ++ "(Just autoSkill) " ++ (stringifyAction act) ++ " soulSkill")
theSuffixPart v False act = ((foldr (++) "" (zipWith f v (map (++ "Skill") skills))) ++ "Nothing " ++ (stringifyAction act) ++ " soulSkill")

thePrefixPart : Vect 5 Bool -> Bool -> PossibleActions -> String
thePrefixPart v b act = (foldr (++) "" (zipWith h v skills)) ++ (if b then "\"auto\" \":\" [autoSkill]") ++ () ++ " soul \":\" [soulSkill]" {-haven't dealt with b or act yet... stkill working-}



prefixAndSuffix : Vect 5 Bool -> Bool -> PossibleActions -> (String,String)
prefixAndSuffix v b act = (thePrefixPart v b act,theSuffixPart v b act)




allPossiblePrefixAndSuffix : List (String,String)
allPossiblePrefixAndSuffix = {-(map (\x => prefixAndSuffix x False NoActions) allPossible5) ++
                             (map (\x => prefixAndSuffix x False OneAction) allPossible5) ++
                             (map (\x => prefixAndSuffix x False ManyActions) allPossible5) ++
                             (map (\x => prefixAndSuffix x True NoActions) allPossible5) ++
                             (map (\x => prefixAndSuffix x True OneAction) allPossible5) ++
                                        -}
                             (map (\x => prefixAndSuffix x True ManyActions) allPossible5)






allStrings : List String
allStrings = ?hole


printAll : List String -> IO ()
printAll [] = pure ()
printAll (x::xs) = putStrLn x >>= (\_ => printAll xs)

main : IO ()
main = printAll (map (\x => (fst x) ++ "|" ++ (snd x)) allPossiblePrefixAndSuffix)










