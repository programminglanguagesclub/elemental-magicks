module ParseTree where
import qualified Lexer

import qualified Data.Map.Strict as HashMap



{-
--data Augmented a = Augmented Lexer.SurfaceData a deriving Functor
newtype Augmented a = Augmented {runAugmented :: (Lexer.SurfaceData,a)} deriving Functor

{-
should just use the normal decorated AST thing.

-}

instance Applicative Augmented where
 pure = undefined

{-

instance Applicative TC where
 pure = TC . Right
 (TC (Right f)) <*> (TC (Right x)) = TC . Right . f $ x
 m <*> n = TC . Left $ (getErr m) <> (getErr n)



fuseAugmented



This can be made into an applicative functor
that allows datatypes to be constructed from their components,

with surface data also filled out correctly....

-}
-}


data CarryingSource a =
 CarryingSource Lexer.SurfaceData a
 deriving Show

getSurfaceSyntax' :: CarryingSource a -> String
getSurfaceSyntax' x = syntax
 where (CarryingSource (Lexer.SurfaceData line column syntax) ast) = x

dummySurfaceData :: Lexer.SurfaceData
dummySurfaceData = Lexer.SurfaceData (-1) (-1) "Dummy Surface Syntax"

getSurfaceSyntax :: Lexer.SurfaceData -> String
getSurfaceSyntax surfaceData = surfaceSyntax
 where Lexer.SurfaceData row column surfaceSyntax = surfaceData

unionSurfaceData :: Lexer.SurfaceData -> Lexer.SurfaceData -> Lexer.SurfaceData
unionSurfaceData surfaceData surfaceData' =
 Lexer.SurfaceData row column $ surfaceSyntax ++ " " ++ surfaceSyntax'
 where Lexer.SurfaceData row column surfaceSyntax = surfaceData
       Lexer.SurfaceData _ _ surfaceSyntax' = surfaceData'


getFileSurfaceData :: [Unit] -> [Spell] -> Lexer.SurfaceData
getFileSurfaceData = undefined


getFoo :: [String] -> Set -> [(String,Set)]
getFoo _ _ = error "what is getFoo??"

data File = File [Unit] [Spell]
             deriving Show

data Unit =
 Unit
  String
  (CarryingSource Stats)
  (Maybe (CarryingSource Start))
  (Maybe (CarryingSource End))
  (Maybe (CarryingSource Counter))
  (Maybe (CarryingSource Spawn))
  (Maybe (CarryingSource Death))
  (Maybe (CarryingSource Auto))
  [CarryingSource Action]
  Soul
 deriving Show

data Spell =
 Spell
  Lexer.SurfaceData
  String Knowledge
  Lexer.SurfaceData Skill {-name, school, level, skill-}
 deriving Show

data Skill = AutomaticSkill Lexer.SurfaceData (Maybe (CarryingSource Expr)) (Maybe (CarryingSource Expr)) Automatic
        {-   | NonautomaticSkill Expr Expr Nonautomatic -}
           deriving Show
{-Cost, Condition, skill-}



{-

This is one place where syntatic sugar becomes interesting.

I am creating a data structure which captures the surface level syntax, even though this is a terrible of representing the data for other purposes.

This allows for better errors messages to be printed.


-}


data VariableBindings = VariableBindings Lexer.SurfaceData [([String], Set)]


data Side = Friendly Lexer.SurfaceData
          | Enemy Lexer.SurfaceData
          deriving Show
            

data RelativeSet = Field Lexer.SurfaceData
                 | Hand Lexer.SurfaceData
                 | Graveyard Lexer.SurfaceData
                 | Banished Lexer.SurfaceData
                 | SpawnLocation Lexer.SurfaceData
                 deriving Show


data Set = SimpleSet Lexer.SurfaceData Side RelativeSet
         | UnionSet Lexer.SurfaceData Set Set
         deriving Show

{-
Don't need both condition and nullable expression.


Either skill effects, or automatic should be allowed to have conditions and/or ifs.

-}
data SkillEffect = Assignment Lexer.SurfaceData [CarryingSource Expr] Mutator (CarryingSource Expr)
                 deriving Show

data Nonautomatic = Nonautomatic Lexer.SurfaceData [(String, Set)] (Maybe (CarryingSource Expr)) Automatic Automatic Automatic {-variables, where condition-}
                  | TerminatedSkillComponent
                  deriving Show
data Automatic = Automatic Lexer.SurfaceData [SkillEffect] Nonautomatic
               deriving Show

data Stats = Stats Lexer.SurfaceData Schools Lexer.SurfaceData Lexer.SurfaceData Lexer.SurfaceData Lexer.SurfaceData Lexer.SurfaceData Lexer.SurfaceData Lexer.SurfaceData
           deriving Show
data Start = Start Lexer.SurfaceData Skill
           deriving Show
data End = End Lexer.SurfaceData Skill
           deriving Show
data Counter = Counter Lexer.SurfaceData Skill
            deriving Show
data Spawn = Spawn Lexer.SurfaceData Skill
            deriving Show
data Death = Death Lexer.SurfaceData Skill
            deriving Show
data Auto = Auto Lexer.SurfaceData Skill
           deriving Show
data Action = Action Lexer.SurfaceData Skill
            deriving Show
data Soul = Soul Lexer.SurfaceData Skill
           deriving Show

data Stat = Attack
          | Defense 
          | Speed
          | Range
          | Level
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




data Knowledge = Knowledge Lexer.SurfaceData
               deriving Show


data Schools = NoSchools Lexer.SurfaceData {-tricky to have surface data here as it's nullable...-}
             | OneSchool Lexer.SurfaceData Lexer.SurfaceData
             | TwoSchools Lexer.SurfaceData Lexer.SurfaceData Lexer.SurfaceData
             deriving Show


data Field = StatField Lexer.SurfaceData (CarryingSource Stat) Temporality
           | HpStatField Lexer.SurfaceData HpStat
           | EngagementField Lexer.SurfaceData
           deriving Show

{-Effects that happen to two units simultaneously trigger resulting effects by order of field position, with
ties broken by initiative. That is the significance of assigning two values at the "same" time-}


data Expr = Constant [Char]
          | ThoughtsExpr Side
          | KnowledgeExpr Knowledge Side
          | Self Field
          | Var Field String
          | Sum (CarryingSource Expr) (CarryingSource Expr)
          | Difference (CarryingSource Expr) (CarryingSource Expr)
          | Product (CarryingSource Expr) (CarryingSource Expr)
          | Quotient (CarryingSource Expr) (CarryingSource Expr)
          | Mod (CarryingSource Expr) (CarryingSource Expr)
          | Always {-add more booleans later.....    again... nullable.-}
          | GT (CarryingSource Expr) (CarryingSource Expr)
          | GEQ (CarryingSource Expr) (CarryingSource Expr)
          | LT (CarryingSource Expr) (CarryingSource Expr)
          | LEQ (CarryingSource Expr) (CarryingSource Expr)
          | EQ (CarryingSource Expr) (CarryingSource Expr)
          | And (CarryingSource Expr) (CarryingSource Expr)
          | Or (CarryingSource Expr) (CarryingSource Expr)
          | Not (CarryingSource Expr)
           deriving Show



getTokens :: String -> [Lexer.Token] {-For now, no error handling-}
getTokens s = case Lexer.runAlex s Lexer.gather of Left _ -> []
                                                   Right x -> x {-(map fst x)-}

extractSurface :: Lexer.Token -> String
extractSurface (Lexer.Token _ (Lexer.SurfaceData _ _ s)) = s

extractSurfaceData :: Lexer.Token -> Lexer.SurfaceData
extractSurfaceData (Lexer.Token _ surfaceData) = surfaceData

prettyPrint :: [String] -> String
prettyPrint [] = ""
prettyPrint (x:[]) = x
prettyPrint (x1:x2:xs) = x1 ++ " " ++ (prettyPrint (x2:xs))


generateTokenLocation :: [Lexer.Token] -> HashMap.Map (Int,Int) String
generateTokenLocation [] = HashMap.empty
generateTokenLocation ((Lexer.Token tokenValue (Lexer.SurfaceData line column surface)):xs) = HashMap.insert (line,column) surface (generateTokenLocation xs)









