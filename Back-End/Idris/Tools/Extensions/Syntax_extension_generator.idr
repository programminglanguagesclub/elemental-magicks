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

g : String -> String
g s = s ++ " \":\" [" ++ s ++ "Skill] "

h : Bool -> String -> String
h True s = g s
h False _ = ""

lb : List Bool
lb = [True,False]
allPossible5 : List (Vect 5 Bool) 
allPossible5 = [[x1,x2,x3,x4,x5] | x1 <- lb, x2 <- lb, x3 <- lb, x4 <- lb, x5 <- lb]

theSuffixPart : Vect 5 Bool -> Bool -> PossibleActions -> String
theSuffixPart v True act = ((foldr (++) "" (zipWith f v (map (++ "Skill") skills))) ++ "(Just autoSkill) " ++ (stringifyAction act) ++ " soulSkill")
theSuffixPart v False act = ((foldr (++) "" (zipWith f v (map (++ "Skill") skills))) ++ "Nothing " ++ (stringifyAction act) ++ " soulSkill")

thePrefixPart : Vect 5 Bool -> Bool -> PossibleActions -> String
thePrefixPart v b act = (foldr (++) "" (zipWith h v skills)) ++ (if b then "\"auto\" \":\" [autoSkill] " else "") ++ ("") ++ (stringifyActionPrefix act) ++ "soul \":\" [soulSkill]"


prefixAndSuffix : Vect 5 Bool -> Bool -> PossibleActions -> (String,String)
prefixAndSuffix v b act = (thePrefixPart v b act,theSuffixPart v b act)

allPossiblePrefixAndSuffix : List (String,String)
allPossiblePrefixAndSuffix = (map (\x => prefixAndSuffix x False NoActions) allPossible5) ++
                             (map (\x => prefixAndSuffix x False OneAction) allPossible5) ++
                             (map (\x => prefixAndSuffix x False TwoActions) allPossible5) ++
                             (map (\x => prefixAndSuffix x False ThreeActions) allPossible5) ++
                             (map (\x => prefixAndSuffix x False FourActions) allPossible5) ++
                             (map (\x => prefixAndSuffix x True NoActions) allPossible5) ++
                             (map (\x => prefixAndSuffix x True OneAction) allPossible5) ++
                             (map (\x => prefixAndSuffix x True TwoActions) allPossible5) ++
                             (map (\x => prefixAndSuffix x True ThreeActions) allPossible5) ++
                             (map (\x => prefixAndSuffix x True FourActions) allPossible5)

printAll : List String -> IO ()
printAll [] = pure ()
printAll (x::xs) = putStrLn (x ++ "\n") >>= (\_ => printAll xs)

main : IO ()
main = printAll (map (\x => stats ++ x) (map (\x => (fst x) ++ " =\n" ++ "MkMonsterFactory (MkBasicMonsterFactory unit_name schools (>> hp <<) (>> attack <<) (>> defense <<) (>> speed <<) (>> range <<) (>> level <<) (>> soulPoints <<)) " ++ (snd x)) allPossiblePrefixAndSuffix))










