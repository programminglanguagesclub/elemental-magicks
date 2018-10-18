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
cabal install edit-distance


levenshteinDistance

defaultEditCosts :: EditCosts

levenshteinDistance :: EditCosts -> String -> String -> Int

Find the Levenshtein edit distance between two strings. That is to say, the number of deletion, insertion and substitution operations that are required to make the two strings equal. Note that this algorithm therefore does not make use of the transpositionCost field of the costs. See also: http://en.wikipedia.org/wiki/Levenshtein_distance.

-}
-------------------------------------------------------------------------------
data File
 = File [Unit] [Spell]
 deriving Show
-------------------------------------------------------------------------------
data Unit 
 = Unit
    String
    Stats
    (Maybe Start)
    (Maybe End)
    (Maybe Counter)
    (Maybe SpawnUnit)
    (Maybe Death)
    (Maybe Auto)
    [Action]
    Soul
 deriving Show
-------------------------------------------------------------------------------
data Spell
 = Spell SurfaceData String Knowledge BaseLevel Skill {- name, school, level, skill -}
 deriving Show
-------------------------------------------------------------------------------
data Skill
 = Skill SurfaceData (Maybe RInt) (Maybe RBool) Automatic
{-Currently no check against this cost being negative. Also doesn't have to be a constant (design decision)-}
 deriving Show
-------------------------------------------------------------------------------
data Start
 = Start SurfaceData Skill
 deriving Show
-------------------------------------------------------------------------------
data End
 = End SurfaceData Skill
 deriving Show
-------------------------------------------------------------------------------
data Counter
 = Counter SurfaceData Skill
 deriving Show
-------------------------------------------------------------------------------
data SpawnUnit
 = SpawnUnit SurfaceData Skill
 deriving Show
-------------------------------------------------------------------------------
data Death
 = Death SurfaceData Skill
 deriving Show
-------------------------------------------------------------------------------
data Auto
 = Auto SurfaceData Skill
 deriving Show
-------------------------------------------------------------------------------
data Action
 = Action SurfaceData Skill
 deriving Show
-------------------------------------------------------------------------------
data Soul
 = Soul SurfaceData Skill
 deriving Show
-------------------------------------------------------------------------------
data RInt
 = RConstant SurfaceData Int
 | RThoughts SurfaceData ParseTree.Side
 | RKnowledge SurfaceData Knowledge ParseTree.Side
 | RSelfProjection SurfaceData LStat {-disallow base for self. allow for var (because var can be quantified.-}
 | RVarProjection SurfaceData RStat Variable
 | RSum SurfaceData RInt RInt
 | RDifference SurfaceData RInt RInt
 | RProduct SurfaceData RInt RInt
 | RQuotient SurfaceData RInt RInt
 | RMod SurfaceData RInt RInt
 | RCardinality SurfaceData String ParseTree.Set RBool
-- eventually set should nest its own conditions so I can have multiple conditions over a union
-- but for now just have the condition here
 deriving Show
-------------------------------------------------------------------------------
data RBool
 = RAlways SurfaceData
 | RGT SurfaceData RInt RInt
 | RGEQ SurfaceData RInt RInt
 | RLT SurfaceData RInt RInt
 | RLEQ SurfaceData RInt RInt
 | REQ SurfaceData RInt RInt
 | RAnd SurfaceData RBool RBool
 | ROr SurfaceData RBool RBool
 | RNot SurfaceData RBool
 | Dead SurfaceData Variable
 | VarInRangeSelf Variable
 | SelfInRangeVar Variable
 | VarInRangeVar Variable Variable
 deriving Show
-------------------------------------------------------------------------------
data Knowledge
 = Earth SurfaceData
 | Fire SurfaceData
 | Water SurfaceData
 | Air SurfaceData
 | Spirit SurfaceData
 | Void SurfaceData
 deriving Show
-------------------------------------------------------------------------------
data SkillEffects
 = SkillEffectList [SkillEffect]
 | SkillEffectConditional RBool SkillEffects SkillEffects -- First list executed if Expr is true. Second executed after.
 | SkillEffectConditionalBranch RBool SkillEffects SkillEffects SkillEffects  -- First list executed if Expr is true, otherwise second is executed. Third is executed after.
 deriving Show
-------------------------------------------------------------------------------
instance Eq Knowledge where
 Earth _ == Earth _ = True
 Fire _ == Fire _ = True
 Water _ == Water _ = True
 Air _ == Air _ = True
 Spirit _ == Spirit _ = True
 Void _ == Void _ = True
 _ == _ = False
-------------------------------------------------------------------------------
typeCheckSkillEffect ::
 Context ->
 ParseTree.SkillEffect ->
 TC SkillEffect

typeCheckSkillEffect context skillEffect =
 case skillEffect of
  ParseTree.Assignment surfaceData lExprs mutator rExpr ->
   SkillEffectAssignment
   <$> typeCheckAssignment context lExprs mutator rExpr
  ParseTree.Revive surfaceData variableName ->
   pure $
   SkillEffectRevive surfaceData variableName -- does not check variable name length currently, although maybe this caught earlier
  ParseTree.DamageSelf surfaceData damage ->
   SkillEffectDamageSelf surfaceData
   <$> typeCheckRInt context damage
  ParseTree.DamageVar surfaceData var damage -> -- again not typechecking var
   SkillEffectDamageVar surfaceData var
   <$> typeCheckRInt context damage
  ParseTree.SendVarToGraveyard surfaceData var ->
   pure $
   SendVarToGraveyard surfaceData var -- not typechecking var currently
  ParseTree.SendSelfToGraveyard surfaceData ->
   pure $
   SendSelfToGraveyard surfaceData
  ParseTree.DamageSquare surfaceData fieldLocation damage ->
   DamageSquare surfaceData {-fieldLocation-} undefined {- should typecheck square value -}
   <$> typeCheckRInt context damage
  ParseTree.DamageRowVar surfaceData var side damage ->
   DamageRowVar surfaceData undefined side
   <$> typeCheckRInt context damage
  ParseTree.DamageColumnVar surfaceData var side damage ->
   DamageColumnVar surfaceData undefined side
   <$> typeCheckRInt context damage
  ParseTree.DamageRowSelf surfaceData damage ->
   DamageRowSelf surfaceData
   <$> typeCheckRInt context damage
  ParseTree.DamageColumnSelf surfaceData damage ->
   DamageColumnSelf surfaceData
   <$> typeCheckRInt context damage
  ParseTree.DamageAllLeftVarExclusive surfaceData var damage ->
   DamageAllLeftVarExclusive surfaceData
   <$> undefined -- typeCheckVariable context (undefined{-Variable undefined var-})
   <*> typeCheckRInt context damage
  ParseTree.DamageAllRightVarExclusive surfaceData var damage -> undefined
  ParseTree.DamageAllBehindVarExclusive surfaceData var damage -> undefined
  ParseTree.DamageAllInFrontVarExclusive surfaceData var damage -> undefined
  ParseTree.DamageAllLeftVarInclusive surfaceData var damage -> undefined
  ParseTree.DamageAllRightVarInclusive surfaceData var damage -> undefined
  ParseTree.DamageAllBehindVarInclusive surfaceData var damage -> undefined
  ParseTree.DamageAllInFrontOfVarInclusive surfaceData var damage -> undefined
  ParseTree.DamageAllLeftSelfExclusive surfaceData damage ->
   DamageAllLeftSelfExclusive surfaceData
   <$> typeCheckRInt context damage
  ParseTree.DamageAllRightSelfExclusive surfaceData damage ->
   DamageAllRightSelfExclusive surfaceData
   <$> typeCheckRInt context damage
  ParseTree.DamageAllBehindSelfExclusive surfaceData damage ->
   DamageAllBehindSelfExclusive surfaceData
   <$> typeCheckRInt context damage
  ParseTree.DamageAllInFrontSelfExclusive surfaceData damage ->
   DamageAllInFrontSelfExclusive surfaceData
   <$> typeCheckRInt context damage
  ParseTree.DamageAllLeftSelfInclusive surfaceData damage ->
   DamageAllLeftSelfInclusive surfaceData
   <$> typeCheckRInt context damage
  ParseTree.DamageAllRightSelfInclusive surfaceData damage ->
   DamageAllRightSelfInclusive surfaceData
   <$> typeCheckRInt context damage
  ParseTree.DamageAllBehindSelfInclusive surfaceData damage ->
   DamageAllBehindSelfInclusive surfaceData
   <$> typeCheckRInt context damage
  ParseTree.DamageAllInFrontOfSelfInclusive surfaceData damage ->
   DamageAllInFrontOfSelfInclusive surfaceData
   <$> typeCheckRInt context damage
  ParseTree.DamageRowOne side surfaceData damage -> undefined
  ParseTree.DamageRowTwo side surfaceData damage -> undefined
  ParseTree.DamageRowThree side surfaceData damage -> undefined
  ParseTree.DamageColumnOne side surfaceData damage -> undefined
  ParseTree.DamageColumnTwo side surfaceData damage -> undefined
  ParseTree.DamageColumnThree side surfaceData damage -> undefined




{-
SendVarToGraveyard : send var to graveyard {SendVarToGraveyard dummySurfaceData $2}
SendSelfToGraveyard : send self to graveyard {SendSelfToGraveyard dummySurfaceData}
-}

-------------------------------------------------------------------------------
typeCheckAssignment ::
 Context ->
 [ParseTree.CarryingSource ParseTree.Expr] ->
 ParseTree.Mutator ->
 ParseTree.CarryingSource ParseTree.Expr ->
 TC Assignment

typeCheckAssignment context lExprs mutator rExpr =
 Assignment
 <$> traverse (typeCheckLInt context) lExprs
 <*> pure mutator
 <*> typeCheckRInt context rExpr
-------------------------------------------------------------------------------
data Automatic
 = Automatic SurfaceData SkillEffects Nonautomatic
 | Universal SurfaceData Judgment RBool SkillEffects Nonautomatic
 deriving Show
-------------------------------------------------------------------------------
data Nonautomatic
 = Selection SurfaceData [Judgment] (Maybe RBool) Automatic Automatic Automatic
 | TerminatedSkillComponent
 | Next Automatic
 deriving Show
-------------------------------------------------------------------------------
data Stats
 = Stats
    SurfaceData
    Schools
    BaseLevel
    BaseHp
    BaseAttack
    BaseDefense
    BaseSpeed
    BaseRange
    BaseSoulPoints
 deriving Show
-------------------------------------------------------------------------------
data Schools
 = NoSchools
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
-------------------------------------------------------------------------------
data BaseLevel
 = BaseLevel SurfaceData Int
 deriving Show
-------------------------------------------------------------------------------
data BaseHp
 = BaseHp SurfaceData Int
 deriving Show
-------------------------------------------------------------------------------
data BaseAttack
 = BaseAttack SurfaceData Int
 deriving Show
-------------------------------------------------------------------------------
data BaseDefense
 = BaseDefense SurfaceData Int
 deriving Show
-------------------------------------------------------------------------------
data BaseSpeed
 = BaseSpeed SurfaceData Int
 deriving Show
-------------------------------------------------------------------------------
data BaseRange
 = BaseRange SurfaceData Int
 deriving Show
-------------------------------------------------------------------------------
data BaseSoulPoints
 = BaseSoulPoints SurfaceData Int
 deriving Show
-------------------------------------------------------------------------------

{- bad -}
data Judgment = Judgment (Variable,ParseTree.Set)
               deriving Show
-------------------------------------------------------------------------------

mkJudgment (string, set) =
 Judgment (Variable (Lexer.SurfaceData 0 0 "DUMMY") string, set)
-------------------------------------------------------------------------------
data Variable
 = Variable SurfaceData String {-String of length 1-}
 deriving Show
-------------------------------------------------------------------------------
data Context
 = EmptyContext
 | ExtendContext Context Judgment
 deriving Show
-------------------------------------------------------------------------------
varIn ::
 Variable ->
 Context ->
 Bool

varIn var context =
 case context of
  EmptyContext -> False
  ExtendContext context' judgment ->
   (varMatches var var') || (varIn var context')
   
   where Judgment (var', _) = judgment
-------------------------------------------------------------------------------
varMatches :: Variable -> Variable -> Bool
varMatches var1 var2 = varName1 == varName2
 
 where Variable _ varName1 = var1
       Variable _ varName2 = var2
-------------------------------------------------------------------------------

{-does not display surface data in error message-}
tryVarPut :: Variable -> ParseTree.Set -> Context -> TC Context
tryVarPut var set EmptyContext = pure (ExtendContext EmptyContext (Judgment (var, set)))
tryVarPut var set (ExtendContext context judgment) =
 if varMatches var var2
  then
   TC $
   Left $
   ["variable "
    ++ varName
    ++ " already bound to "
    ++ (show set2)]
  else
   pure $
   ExtendContext (ExtendContext context judgment) $
   Judgment (var,set)
 where (Judgment (var2, set2)) = judgment
       (Variable _ varName) = var
-------------------------------------------------------------------------------
tryExtendContext :: Context -> Judgment -> TC Context
tryExtendContext context judgment = tryVarPut variable set context
 
 where Judgment (variable, set) = judgment
-------------------------------------------------------------------------------
tryExtendContextMultiple ::
 Context ->
 [Judgment] ->
 TC Context

tryExtendContextMultiple context [] = pure context
tryExtendContextMultiple context (x:xs) =
 joinTC $
 tryExtendContextMultiple
 <$> tryExtendContext context x
 <*> pure xs
-------------------------------------------------------------------------------
data LStat
 = LStatField LModifier ParseTree.Stat
 | LHpStat LHpStat
 | LEngagement
 deriving Show
-------------------------------------------------------------------------------
data LModifier
 = LTemporary
 | LPermanent
 deriving Show
-------------------------------------------------------------------------------
data LHpStat
 = LCurrentHp
 | LMaxHp
 deriving Show
-------------------------------------------------------------------------------
data RStat
 = RStat ParseTree.Temporality ParseTree.Stat
 | RHpStat RHpStat
 | REngagement -- same as LEngagement...
{-unimplemented. Like LStat but allows reference to base-}
 deriving Show
-------------------------------------------------------------------------------
data RHpStat
 = RCurrentHp
 | RMaxHp
 | RBaseHp
 deriving Show
-------------------------------------------------------------------------------
removeSurface :: ParseTree.CarryingSource a -> a
removeSurface (ParseTree.CarryingSource surfaceData a) = a
-------------------------------------------------------------------------------
typeCheckLModifier :: ParseTree.Temporality -> TC LModifier
typeCheckLModifier temporality =
 case temporality of
  ParseTree.Temporary surfaceData -> pure $ LTemporary
  ParseTree.Permanent surfaceData -> pure $ LPermanent
  ParseTree.Base surfaceData ->
   TC $
   Left
    ["Attempted assignment to base value in subexpression:\n" ++
     (getSurfaceSyntax surfaceData) ++
     (getLocationMessage surfaceData)]
-------------------------------------------------------------------------------
typeCheckLStat :: ParseTree.Field -> TC LStat
typeCheckLStat field =
 case field of
  ParseTree.StatField surfaceData stat temporality ->
   joinTC $
   pure $
   LStatField
   <$> typeCheckLModifier temporality
   <*> (pure $ removeSurface stat)
  ParseTree.HpStatField surfaceData hpStat ->
   case hpStat of
    (ParseTree.CarryingSource surfaceData' ParseTree.CurrentHp) ->
     pure $ LHpStat LCurrentHp
    (ParseTree.CarryingSource surfaceData' ParseTree.MaxHp) ->
     pure $ LHpStat LMaxHp
  ParseTree.EngagementField surfaceData ->
   pure LEngagement
-------------------------------------------------------------------------------
typeCheckRStatSelf :: ParseTree.Field -> TC LStat
typeCheckRStatSelf = typeCheckLStat
-------------------------------------------------------------------------------
typeCheckRStatVar :: ParseTree.Field -> TC RStat
typeCheckRStatVar field =
 case field of
  ParseTree.StatField surfaceData (ParseTree.CarryingSource surfaceData' stat) temporality ->
   pure $ RStat temporality stat
  ParseTree.HpStatField surfaceData hpStat ->
   case hpStat of
    (ParseTree.CarryingSource surfaceData' ParseTree.CurrentHp) ->
     pure $ RHpStat RCurrentHp
    (ParseTree.CarryingSource surfaceData' ParseTree.MaxHp) ->
     pure $ RHpStat RMaxHp
    (ParseTree.CarryingSource surfaceData' ParseTree.BaseHp) ->
     pure $ RHpStat RBaseHp
-- don't check for assigning from a soul card, which is not allowed...
  ParseTree.EngagementField surfaceData -> pure REngagement
-------------------------------------------------------------------------------
{-NONE OF THESE CURRENTLY ACCOUNT FOR CARDS NOT BEING ON THE FIELD. E.G., WE SHOULD NOT TARGET THE MODIFIED STATS OF CARDS IN SPAWN-}

-------------------------------------------------------------------------------
data SkillEffect
 = SkillEffectAssignment Assignment {-I need more skill effects, of course-}
 | SimultaneousSkillEffect Lexer.SurfaceData [SkillEffect] {- This is mostly to make the interface better: Instead of showing the effects happening one after another, they happen at the same time. Note that death, deathskills, counterskills, etc, then happen according to field position, initiative-}

 -- There are huge restrictions on simultaneous effects: In particular, none of the skill effects can depend on any of the other skill effects (order performed cannon possibly change outcome)


{-

If certain things cause skills to stop, such as targets not existing, I need to make sure this does not stop a simultaneous effect from executing mid-way through.

-}


 | SkillEffectRevive Lexer.SurfaceData String
 | SkillEffectDamageSelf Lexer.SurfaceData RInt
 | SkillEffectDamageVar Lexer.SurfaceData String RInt
 | SendVarToGraveyard Lexer.SurfaceData String
 | SendSelfToGraveyard Lexer.SurfaceData
 | DamageSquare Lexer.SurfaceData FieldLocation RInt -- This string is teh square... should really have its own type
 | DamageRowVar Lexer.SurfaceData String ParseTree.Side RInt
 | DamageColumnVar Lexer.SurfaceData String ParseTree.Side RInt
 | DamageRowSelf Lexer.SurfaceData RInt
 | DamageColumnSelf Lexer.SurfaceData RInt
 | DamageAllLeftVarExclusive Lexer.SurfaceData String RInt
 | DamageAllRightVarExclusive Lexer.SurfaceData String RInt
 | DamageAllBehindVarExclusive Lexer.SurfaceData String RInt
 | DamageAllInFrontVarExclusive Lexer.SurfaceData String RInt
 | DamageAllLeftVarInclusive Lexer.SurfaceData String RInt
 | DamageAllRightVarInclusive Lexer.SurfaceData String RInt
 | DamageAllBehindVarInclusive Lexer.SurfaceData String RInt
 | DamageAllInFrontOfVarInclusive Lexer.SurfaceData String RInt
 | DamageAllLeftSelfExclusive Lexer.SurfaceData RInt
 | DamageAllRightSelfExclusive Lexer.SurfaceData RInt
 | DamageAllBehindSelfExclusive Lexer.SurfaceData RInt
 | DamageAllInFrontSelfExclusive Lexer.SurfaceData RInt
 | DamageAllLeftSelfInclusive Lexer.SurfaceData RInt
 | DamageAllRightSelfInclusive Lexer.SurfaceData RInt
 | DamageAllBehindSelfInclusive Lexer.SurfaceData RInt
 | DamageAllInFrontOfSelfInclusive Lexer.SurfaceData RInt
 {-| DamageRowOne ParseTree.Side Lexer.SurfaceData RInt
 | DamageRowTwo ParseTree.Side Lexer.SurfaceData RInt
 | DamageRowThree ParseTree.Side Lexer.SurfaceData RInt
 | DamageColumnOne ParseTree.Side Lexer.SurfaceData RInt
 | DamageColumnTwo ParseTree.Side Lexer.SurfaceData RInt
 | DamageColumnThree ParseTree.Side Lexer.SurfaceData RInt
-} -- these can be included as simultaneous effects




 deriving Show
-------------------------------------------------------------------------------
data Assignment -- why does this exist as a separate datatype rather than just being part of skilleffect?
 = Assignment [LExpr] ParseTree.Mutator RInt
 deriving Show
-------------------------------------------------------------------------------

{-
 - data Side
 -  = Friendly Lexer.SurfaceData -- friendly relative to evoker
 -   | Enemy Lexer.SurfaceData -- enemy relative to evoker
 -    | FriendlyVar Lexer.SurfaceData -- friendly relative to variable
 -     | EnemyVar Lexer.SurfaceData -- enemy relative to varaible
 -      deriving Show
 -      -}

data Side
 = Friendly Lexer.SurfaceData -- friendly relative to evoker
 | Enemy Lexer.SurfaceData -- enemy relative to evoker
 | FriendlyVar Variable Lexer.SurfaceData -- friendly relative to variable
 | EnemyVar Variable Lexer.SurfaceData -- enemy relative to varaible
 deriving Show


data FieldLocation
 = OneFL Side
 | TwoFL Side
 | ThreeFL Side
 | FourFL Side
 | FiveFL Side
 | SixFL Side
 | SevenFL Side
 | EightFL Side
 | NineFL Side
 | OnSameSquare Variable -- originally given side; typechecker will make sure variable is the opposite side as the side data listed here.
 | ToTheLeftOf Side Variable
 | ToTheRightOf Side Variable
 | InFrontOf Side Variable
 | Behind Side Variable 
 | OnSameSquareSelf -- refers to enemy of course
 | ToTheLeftOfSelf Side
 | ToTheRightOfSelf Side
 | InFrontOfSelf Side
 | BehindSelf Side
 deriving Show


typecheckSide :: Context -> ParseTree.Side -> TC Side
typecheckSide context side =
 case side of
  ParseTree.Friendly surfaceData -> pure $ Friendly surfaceData
  ParseTree.Enemy surfaceData -> pure $ Enemy surfaceData
  ParseTree.FriendlyVar var surfaceData -> joinTC $ pure $ FriendlyVar <$> typeCheckVariable context (Variable undefined var) <*> pure surfaceData
  ParseTree.EnemyVar var surfaceData -> undefined
-------------------------------------------------------------------------------
typecheckFieldLocation :: Context -> ParseTree.FieldLocation -> TC FieldLocation
typecheckFieldLocation context fieldLocation =
 case fieldLocation of
   ParseTree.OneFL side ->
    OneFL
    <$> typecheckSide context side
   ParseTree.TwoFL side ->
    TwoFL
    <$> typecheckSide context side
   ParseTree.ThreeFL side ->
    ThreeFL
    <$> typecheckSide context side
   ParseTree.FourFL side ->
    FourFL
    <$> typecheckSide context side
   ParseTree.FiveFL side ->
    FiveFL
    <$> typecheckSide context side
   ParseTree.SixFL side ->
    SixFL
    <$> typecheckSide context side
   ParseTree.SevenFL side ->
    SevenFL
    <$> typecheckSide context side
   ParseTree.EightFL side ->
    EightFL
    <$> typecheckSide context side
   ParseTree.NineFL side ->
    NineFL
    <$> typecheckSide context side
   ParseTree.OnSameSquare var ->
    OnSameSquare
    <$> typeCheckVariable context (Variable undefined var)
   ParseTree.ToTheLeftOf side var ->
    ToTheLeftOf
    <$> typecheckSide context side
    <*> typeCheckVariable context (Variable undefined var)
   ParseTree.ToTheRightOf side var ->
    ToTheRightOf
    <$> typecheckSide context side
    <*> typeCheckVariable context (Variable undefined var)
   ParseTree.InFrontOf side var ->
    InFrontOf
    <$> typecheckSide context side
    <*> typeCheckVariable context (Variable undefined var)
   ParseTree.Behind side var ->
    Behind
    <$> typecheckSide context side
    <*> typeCheckVariable context (Variable undefined var)
   ParseTree.OnSameSquareSelf ->
    pure OnSameSquareSelf -- refers to enemy of course
   ParseTree.ToTheLeftOfSelf side ->
    ToTheLeftOfSelf
    <$> typecheckSide context side
   ParseTree.ToTheRightOfSelf side ->
    ToTheRightOfSelf
    <$> typecheckSide context side
   ParseTree.InFrontOfSelf side ->
    InFrontOfSelf
    <$> typecheckSide context side
   ParseTree.BehindSelf side ->
    BehindSelf
    <$> typecheckSide context side

-- Stuff for affecting entire rows or columns to be dealt with elsewhere.
-- This just indexes a particular square.
-------------------------------------------------------------------------------
data LExpr
 = LThoughtsExpr SurfaceData ParseTree.Side
 | LKnowledgeExpr SurfaceData Knowledge ParseTree.Side
 | LSelfProjection SurfaceData LStat {-should exclude base stats, soul points...-}
 | LVarProjection SurfaceData Variable LStat
 | LFieldProjection SurfaceData FieldLocation
 {- Cardinality not implemented... -}
 deriving Show
-------------------------------------------------------------------------------
getDistance :: String -> String -> Int
getDistance s1 s2 = restrictedDamerauLevenshteinDistance defaultEditCosts s1 s2
-------------------------------------------------------------------------------
getDistances :: String -> [String] -> [Int]
getDistances s1 ss = map (getDistance s1) ss
-------------------------------------------------------------------------------
getDistancesWithStrings :: String -> [String] -> [(String,Int)]
getDistancesWithStrings s1 ss = zip ss $ map (getDistance s1) ss
-------------------------------------------------------------------------------
getCloseDistancesWithStrings :: String -> [String] -> [(String,Int)]
getCloseDistancesWithStrings s1 ss = filter (\x -> (snd x) <= 2) $ getDistancesWithStrings s1 ss
-------------------------------------------------------------------------------
getDistanceMessages'' :: Maybe String -> String
getDistanceMessages'' x =
 case x of
  Nothing -> ""
  Just s -> s
-------------------------------------------------------------------------------
getDistanceMessages' :: [(String,Int)] -> Maybe String
getDistanceMessages' [] = Nothing
getDistanceMessages' (x1:x2:xs) = Just ((fst x1) ++ " or " ++ ( getDistanceMessages'' $ getDistanceMessages' (x2:xs)))
getDistanceMessages' (x:[]) = Just ( (fst x) ++ "?")
-------------------------------------------------------------------------------
getDistanceMessages :: String -> [String] -> [String]
getDistanceMessages s ss =
 let x = getCloseDistancesWithStrings s ss in
  case getDistanceMessages' x of
   Nothing -> []
   Just y -> [y]
-------------------------------------------------------------------------------
schoolFromKnowledge :: Knowledge -> Schools
schoolFromKnowledge knowledge =
 case knowledge of
  Earth surfaceData -> EarthMono surfaceData
  Fire surfaceData -> FireMono surfaceData
  Water surfaceData -> WaterMono surfaceData
  Air surfaceData -> AirMono surfaceData
  Spirit surfaceData -> SpiritMono surfaceData
  Void surfaceData -> VoidMono surfaceData
-------------------------------------------------------------------------------
errorPrefix :: Int -> Int -> String
errorPrefix line column = "Syntax error on line " ++ (show line) ++ ", column " ++ (show column) ++ ": "
-------------------------------------------------------------------------------
errorPrefix' :: Lexer.SurfaceData -> String
errorPrefix' (Lexer.SurfaceData line column surface) = errorPrefix line column
-------------------------------------------------------------------------------
typeCheckKnowledge :: ParseTree.Knowledge -> TC Knowledge
typeCheckKnowledge knowledge =
 typeCheckSchool surfaceData
 where ParseTree.Knowledge surfaceData = knowledge
-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------
showKnowledge :: Knowledge -> String
showKnowledge knowledge =
 case knowledge of
  Earth surfaceData -> "earth"
  Fire surfaceData -> "fire"
  Water surfaceData -> "water"
  Air surfaceData -> "air"
  Spirit surfaceData -> "spirit"
  Void surfaceData -> "void"
-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------
schoolsFromKnowledge :: Lexer.SurfaceData -> Knowledge -> Knowledge -> TC Schools
schoolsFromKnowledge surfaceData school1 school2 =
 case (school1,school2) of
  (Earth surfaceData1, Fire surfaceData2) ->
   pure . EarthFire $ surfaceData
  (Earth surfaceData1, Water surfaceData2) ->
   pure . EarthWater $ surfaceData
  (Earth surfaceData1, Air surfaceData2) ->
   pure . EarthAir $ surfaceData
  (Earth surfaceData1, Spirit surfaceData2) ->
   pure . EarthSpirit $ surfaceData
  (Earth surfaceData1, Void surfaceData2) ->
   pure . EarthVoid $ surfaceData
  (Fire surfaceData1, Water surfaceData2) ->
   pure . FireWater $ surfaceData
  (Fire surfaceData1, Air surfaceData2) ->
   pure . FireAir $ surfaceData
  (Fire surfaceData1, Spirit surfaceData2) ->
   pure . FireSpirit $ surfaceData
  (Fire surfaceData1, Void surfaceData2) ->
   pure . FireVoid $ surfaceData
  (Water surfaceData1, Air surfaceData2) ->
   pure . WaterAir $ surfaceData
  (Water surfaceData1, Spirit surfaceData2) ->
   pure . WaterSpirit $ surfaceData
  (Water surfaceData1, Void surfaceData2) ->
   pure . WaterVoid $ surfaceData
  (Air surfaceData1, Spirit surfaceData2) ->
   pure . AirSpirit $ surfaceData
  (Air surfaceData1, Void surfaceData2) ->
   pure . AirVoid $ surfaceData
  (Spirit surfaceData1, Void surfaceData2) ->
   pure . SpiritVoid $ surfaceData
  (k1,k2) ->
   let ((Lexer.SurfaceData line1 column1 surface1),(Lexer.SurfaceData line2 column2 surface2)) = (getSurface k1, getSurface k2) in
   let prefix = errorPrefix line1 column1 in
   if school1 == school2
    then
     putErr $
     prefix ++
     "Units cannot belong to two identical schools"
    else
     putErr $
     prefix ++
     ("Invalid pair of schools: " ++
      (showKnowledge school1) ++
      " " ++
      (showKnowledge school2) ++
      ". Did you mean " ++
      (showKnowledge school2) ++
      " " ++
      (showKnowledge school1))
-------------------------------------------------------------------------------

{-Need to wrap variable, and not just have String...-}


{-
getSet :: Context -> Variable -> 
-}


{-
 -
 -I HAVE WHERE, BUT I DO NOT HAVE IF STATEMENTS YET IN MY SKILLS BEYOND THIS... I SHOULD.
 - 
 - -}



{-
data SkillEffects
 = SkillEffectList [SkillEffect]
 | SkillEffectConditional RBool SkillEffects SkillEffects -- First list executed if Expr is true. Second executed after.
 | SkillEffectConditionalBranch RBool SkillEffects SkillEffects SkillEffects  -- First list executed if Expr is true, otherwise second is executed. Third is executed after.
 deriving Show
-}

-------------------------------------------------------------------------------
typecheckSkillEffects :: Context -> ParseTree.SkillEffects -> TC SkillEffects
typecheckSkillEffects context skillEffects =
 case skillEffects of
  ParseTree.SkillEffectList skillEffectList ->
   SkillEffectList
   <$> traverse (typeCheckSkillEffect context) skillEffectList
  ParseTree.SkillEffectConditional condition trueSkillEffects nextSkillEffects ->
   SkillEffectConditional
   <$> undefined --typeCheckRBool context condition
   <*> typecheckSkillEffects context trueSkillEffects
   <*> typecheckSkillEffects context nextSkillEffects
  ParseTree.SkillEffectConditionalBranch condition trueSkillEffects falseSkillEffects nextSkillEffects ->
   SkillEffectConditionalBranch
   <$> undefined -- typeCheckRBool context condition
   <*> typecheckSkillEffects context trueSkillEffects
   <*> typecheckSkillEffects context falseSkillEffects
   <*> typecheckSkillEffects context nextSkillEffects
-------------------------------------------------------------------------------
typeCheckAutomatic :: Context -> ParseTree.Automatic -> TC Automatic
typeCheckAutomatic context (ParseTree.Automatic surfaceData skillEffects nonautomatic) =
 Automatic surfaceData
   <$> typecheckSkillEffects context skillEffects
   <*> typeCheckNonautomatic context nonautomatic
typeCheckAutomatic context (ParseTree.Universal surfaceData binding condition skillEffects nonautomatic) =
 Universal surfaceData
 <$> (pure judgment)
 <*>
  (joinTC $
   typeCheckRBool
   <$> context'
   <*> pure condition)
 <*> typecheckSkillEffects context skillEffects
 <*> typeCheckNonautomatic context nonautomatic
 where judgment = mkJudgment binding
       context' = tryExtendContext context judgment


{-
 Automatic surfaceData
 <$> traverse (typeCheckSkillEffect context) skillEffects
 <*> typeCheckNonautomatic context nonautomatic
typeCheckAutomatic context (ParseTree.Universal surfaceData binding condition skillEffects nonautomatic) =
 Universal surfaceData
 <$> (pure judgment)
 <*>
  (joinTC $
   typeCheckRBool
   <$> context'
   <*> pure condition)
 <*>
  (joinTC $
   traverse
   <$> (typeCheckSkillEffect <$> context')
   <*> pure skillEffects)
 <*> typeCheckNonautomatic context nonautomatic
 where judgment = mkJudgment binding
       context' = tryExtendContext context judgment
-}
-------------------------------------------------------------------------------

{-need to make sure that I check the new bindings somewhere to make sure that variable names are no more than 1 character long-}
-------------------------------------------------------------------------------
typeCheckNonautomatic :: Context -> ParseTree.Nonautomatic -> TC Nonautomatic
typeCheckNonautomatic context nonautomatic =
 --trace (show context) $
 case nonautomatic of
  ParseTree.Nonautomatic surfaceData newBindings condition thenBranch elseBranch nextBranch ->
   case tryExtendContextMultiple context (map mkJudgment newBindings) of
    TC (Right context') -> 
     --trace (show newBindings) $
     Selection surfaceData (map mkJudgment newBindings)
     <$> typeCheckCondition condition context'
     <*> typeCheckAutomatic context' thenBranch
     <*> typeCheckAutomatic context elseBranch
     <*> typeCheckAutomatic context nextBranch
    TC (Left errors) -> TC (Left errors)
  ParseTree.TerminatedSkillComponent -> pure TerminatedSkillComponent -- don't make sure all of context used yet or anything.
  ParseTree.Next automatic ->
   joinTC $ pure $ Next <$> typeCheckAutomatic context automatic


-- I want to make sure that binding errors aren't reported more than they should be..
-------------------------------------------------------------------------------

{-
Also need to know that variables in the next branch are disjoint from the then and else branches 
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
typeCheckVariable context var =
 case varIn var context of
  True -> pure var
  False ->
   TC $
   Left $
   ["variable " ++ variableName ++" not defined in " ++ (show context) ++ "."]
   where (Variable surfaceData variableName) = var


{-
--unused
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


-}

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
 - THE difference between Judgements in contexts here, and sets in the parser, is for convenience we might want to additionally allow the soul to be an additional area to select, even though
 - we don't want other cards to select that. The reason for that is so that SELF can be bound then to the soul, and we can typecheck references to self.
 -
 - Another possibility is to have a separate "no selfs" clause that soul skills check (this is probably easier).
 -
 - -}


{-Certain effects and conditions are not valid depending on the set. You cannot damage cards in the graveyard, for instance-}
-------------------------------------------------------------------------------
typeCheckSchools :: ParseTree.Schools -> TC Schools
typeCheckSchools (ParseTree.NoSchools) =
 pure $
 NoSchools
typeCheckSchools (ParseTree.OneSchool surfaceData s) =
 schoolFromKnowledge
 <$> typeCheckSchool surfaceData
typeCheckSchools (ParseTree.TwoSchools surfaceData s1 s2) =
 joinTC $
 schoolsFromKnowledge
 surfaceData
 <$> typeCheckSchool s1
 <*> typeCheckSchool s2
-------------------------------------------------------------------------------
getLocationMessage :: Lexer.SurfaceData -> String
getLocationMessage (Lexer.SurfaceData lineNumber columnNumber _) =
 "on line " ++ (show lineNumber) ++ ", column " ++ (show columnNumber) ++ "\n"
getSurfaceSyntax :: Lexer.SurfaceData -> String
getSurfaceSyntax (Lexer.SurfaceData _ _ surfaceSyntax) = surfaceSyntax ++ "\n"
-------------------------------------------------------------------------------
{-should add isDead to conditions?-}
typeCheckRBool :: Context -> ParseTree.CarryingSource ParseTree.Expr -> TC RBool {-call typeCheckRBool?-}
typeCheckRBool context (ParseTree.CarryingSource surfaceData expr) =
 case expr of
  ParseTree.Constant value ->
   TC $
   Left
    ["Type mismatch between Boolean (required type) and " ++
     "Integer (type of integer literal) in subexpression:\n" ++
     (getSurfaceSyntax surfaceData) ++
     (getLocationMessage surfaceData)]
  ParseTree.Dead var ->
   joinTC $ pure $ Dead surfaceData <$> typeCheckVariable context (Variable ParseTree.dummySurfaceData var)
  ParseTree.ThoughtsExpr side ->                             {-Ignoring distinction between Thought and Thoughts for now-}
    TC $
    Left
     ["Type mismatch between Boolean (required type) and " ++ 
      "Integer (type of thoughts) in subexpression:\n" ++
      (getSurfaceSyntax surfaceData) ++
      (getLocationMessage surfaceData)]
  ParseTree.KnowledgeExpr knowledge side ->
   TC $
   Left
    ["Type mismatch between Boolean (required type) and " ++
     "Integer (type of knowledge) in subexpression:\n" ++
     (getSurfaceSyntax surfaceData) ++
     (getLocationMessage surfaceData)]
  ParseTree.Self field ->
   TC $
   Left
    ["Type mismatch between Boolean (required type) and " ++
     "Integer (type of projection from self) in subexpression:\n" ++
     (getSurfaceSyntax surfaceData) ++
     (getLocationMessage surfaceData)]
  ParseTree.Var field var ->
   TC $
   Left
    ["Type mismatch between Boolean (required type) and " ++
     "Integer (type of projection from variable " ++
     var ++
     ") in subexpression:\n" ++
     (getSurfaceSyntax surfaceData) ++
     (getLocationMessage surfaceData)]
  ParseTree.Sum expr1 expr2 ->
    TC $
    Left
     ["Type mismatch between Boolean (requied type) and " ++
      "Integer (result type of (+) operator) in subexpression:\n" ++
      (getSurfaceSyntax surfaceData) ++
      (getLocationMessage surfaceData)]    
  ParseTree.Difference expr1 expr2 ->
    TC $
    Left
     ["Type mismatch between Boolean (required type) and " ++
      "Integer (result type of (-) operator) in subexpression:\n" ++
      (getSurfaceSyntax surfaceData) ++
      (getLocationMessage surfaceData)]
  ParseTree.Product expr1 expr2 ->
   TC $
   Left
    ["Type mismatch between Boolean (required type) and " ++
     "Integer (result type of (*) operator) in subexpression:\n" ++
     (getSurfaceSyntax surfaceData) ++
     (getLocationMessage surfaceData)]
  ParseTree.Quotient expr1 expr2 ->
   TC $
   Left
    ["Type mismatch between Boolean (required type) and " ++
     "Integer (result type of (/) operator) in subexpression:\n" ++
     (getSurfaceSyntax surfaceData) ++
     (getLocationMessage surfaceData)]
  ParseTree.Mod expr1 expr2 ->                                      {-Have to make sure this implements modulus, not remainder...-}
   TC $
   Left
    ["Type mismatch between Boolean (required type) and " ++
     "Integer (result type of modulus operator) in subexpression:\n" ++
     (getSurfaceSyntax surfaceData) ++
     (getLocationMessage surfaceData)]
  ParseTree.Always ->
   pure $
   RAlways surfaceData
  ParseTree.GT expr1 expr2 ->
   RGT surfaceData
   <$> typeCheckRInt context expr1
   <*> typeCheckRInt context expr2
  ParseTree.GEQ expr1 expr2 ->
   RGEQ surfaceData
   <$> typeCheckRInt context expr1
   <*> typeCheckRInt context expr2
  ParseTree.LT expr1 expr2 ->
   RLT surfaceData
   <$> typeCheckRInt context expr1
   <*> typeCheckRInt context expr2
  ParseTree.LEQ expr1 expr2 ->
   RLEQ surfaceData
   <$> typeCheckRInt context expr1
   <*> typeCheckRInt context expr2
  ParseTree.EQ expr1 expr2 ->
   REQ surfaceData
   <$> typeCheckRInt context expr1
   <*> typeCheckRInt context expr2
  ParseTree.And expr1 expr2 ->
   RAnd surfaceData
   <$> typeCheckRBool context expr1
   <*> typeCheckRBool context expr2
  ParseTree.Or expr1 expr2 ->
   ROr surfaceData
   <$> typeCheckRBool context expr1
   <*> typeCheckRBool context expr2
  ParseTree.Not expr ->
   RNot surfaceData
   <$> typeCheckRBool context expr
  ParseTree.VarInRangeSelf var -> pure $ VarInRangeSelf (Variable ParseTree.dummySurfaceData var)
  ParseTree.SelfInRangeVar var -> pure $ SelfInRangeVar (Variable ParseTree.dummySurfaceData var)
  ParseTree.VarInRangeVar var1 var2 ->
   pure $ VarInRangeVar (Variable ParseTree.dummySurfaceData var1) (Variable ParseTree.dummySurfaceData var2)
-------------------------------------------------------------------------------
typeCheckSkill :: ParseTree.CarryingSource ParseTree.Skill -> TC Skill
typeCheckSkill (ParseTree.CarryingSource surfaceData (ParseTree.AutomaticSkill cost condition automatic)) =
 Skill surfaceData
  <$> typeCheckCost cost
  <*> typeCheckCondition condition EmptyContext
  <*> typeCheckAutomatic EmptyContext automatic
-------------------------------------------------------------------------------
typeCheckCost :: Maybe (ParseTree.CarryingSource ParseTree.Expr) -> TC (Maybe RInt)
typeCheckCost Nothing = pure Nothing
typeCheckCost (Just expr) = Just <$> typeCheckRInt EmptyContext expr
-------------------------------------------------------------------------------
typeCheckCondition :: Maybe (ParseTree.CarryingSource ParseTree.Expr) -> Context -> TC (Maybe RBool)
-- if I have maybe here shouldn't I get rid of always as a condition?

typeCheckCondition Nothing _ = pure Nothing
typeCheckCondition (Just expr) context = Just <$> typeCheckRBool context expr
-------------------------------------------------------------------------------
typeCheckStart :: Maybe (ParseTree.CarryingSource ParseTree.Start) -> TC (Maybe Start)
typeCheckStart Nothing = pure Nothing
typeCheckStart (Just (ParseTree.CarryingSource surfaceData (ParseTree.Start skill))) =
 --trace "typeCheckStart not implemented" $
 -- check engagement conditions? (it doesn't make sense to condition on engagement of self)
 -- similar for other skill types I guess..
 Just
 <$> Start surfaceData
 <$> typeCheckSkill skill
-------------------------------------------------------------------------------
typeCheckEnd :: Maybe (ParseTree.CarryingSource ParseTree.End) -> TC (Maybe End)
typeCheckEnd Nothing = pure Nothing
typeCheckEnd (Just (ParseTree.CarryingSource surfaceData (ParseTree.End skill))) =
 Just
 <$> End surfaceData
 <$> typeCheckSkill skill
-------------------------------------------------------------------------------
typeCheckCounter :: Maybe (ParseTree.CarryingSource ParseTree.Counter) -> TC (Maybe Counter)
typeCheckCounter Nothing = pure Nothing
typeCheckCounter (Just (ParseTree.CarryingSource surfaceData (ParseTree.Counter skill))) =
 -- here we can also check self hp and max hp conditions
 -- same for death
 Just
 <$> Counter surfaceData
 <$> typeCheckSkill skill
-------------------------------------------------------------------------------
typeCheckSpawnUnit :: Maybe (ParseTree.CarryingSource ParseTree.Spawn) -> TC (Maybe SpawnUnit)
typeCheckSpawnUnit Nothing = pure Nothing
typeCheckSpawnUnit (Just (ParseTree.CarryingSource surfaceData (ParseTree.Spawn skill))) =
 Just 
 <$>
  (joinTC $
   noSelfReferences (SpawnUnit surfaceData)
   <$> typeCheckSkill skill)
-------------------------------------------------------------------------------
typeCheckDeath :: Maybe (ParseTree.CarryingSource ParseTree.Death) -> TC (Maybe Death)
typeCheckDeath Nothing = pure Nothing
typeCheckDeath (Just (ParseTree.CarryingSource surfaceData (ParseTree.Death skill))) =
 Just
 <$> Death surfaceData
 <$> typeCheckSkill skill
-------------------------------------------------------------------------------
typeCheckAuto :: Maybe (ParseTree.CarryingSource ParseTree.Auto) -> TC (Maybe Auto)
typeCheckAuto Nothing = pure Nothing
typeCheckAuto (Just (ParseTree.CarryingSource surfaceData (ParseTree.Auto skill))) =
 --trace "typecheckauto not implemented" $ -- could eventually prevent from referencing self engagement? similar for other skill types..
 Just
 <$> Auto surfaceData
 <$> typeCheckSkill skill
-------------------------------------------------------------------------------
typeCheckAction :: ParseTree.CarryingSource ParseTree.Action -> TC Action
typeCheckAction (ParseTree.CarryingSource surfaceData (ParseTree.Action skill)) =
 Action surfaceData
 <$> typeCheckSkill skill
-------------------------------------------------------------------------------
typeCheckActions :: [ParseTree.CarryingSource ParseTree.Action] -> TC [Action]
typeCheckActions = traverse typeCheckAction
-------------------------------------------------------------------------------
noSelfReferencesRBool :: RBool -> TC RBool
noSelfReferencesRBool rBool =
 case rBool of
  RAlways surfaceData -> pure rBool
  RGT surfaceData firstRInt secondRInt ->
   joinTC $
   pure $
   RGT surfaceData
   <$> noSelfReferencesRInt firstRInt
   <*> noSelfReferencesRInt secondRInt
  RGEQ surfaceData firstRInt secondRInt ->
   joinTC $
   pure $
   RGEQ surfaceData
   <$> noSelfReferencesRInt firstRInt
   <*> noSelfReferencesRInt secondRInt
  RLT surfaceData firstRInt secondRInt ->
   joinTC $
   pure $
   RLT surfaceData
   <$> noSelfReferencesRInt firstRInt
   <*> noSelfReferencesRInt secondRInt
  RLEQ surfaceData firstRInt secondRInt ->
   joinTC $
   pure $
   RLEQ surfaceData
   <$> noSelfReferencesRInt firstRInt
   <*> noSelfReferencesRInt secondRInt
  REQ surfaceData firstRInt secondRInt ->
   joinTC $
   pure $
   REQ surfaceData
   <$> noSelfReferencesRInt firstRInt
   <*> noSelfReferencesRInt secondRInt
  RAnd surfaceData firstRBool secondRBool ->
   joinTC $
   pure $
   RAnd surfaceData
   <$> noSelfReferencesRBool firstRBool
   <*> noSelfReferencesRBool secondRBool
  ROr surfaceData firstRBool secondRBool ->
   joinTC $
   pure $
   ROr surfaceData
   <$> noSelfReferencesRBool firstRBool
   <*> noSelfReferencesRBool secondRBool
  RNot surfaceData negatedRBool ->
   joinTC $
   pure $
   RNot surfaceData
   <$> noSelfReferencesRBool negatedRBool
  VarInRangeSelf var ->      TC $
     Left
      ["Invalid reference to self in subexpression:\n"]-- ++
      -- (getSurfaceSyntax surfaceData) ++
      -- (getLocationMessage surfaceData)]
  SelfInRangeVar var ->      TC $
     Left
      ["Invalid reference to self in subexpression:\n"]-- ++
      -- (getSurfaceSyntax surfaceData) ++
      -- (getLocationMessage surfaceData)]
  VarInRangeVar var1 var2 -> pure $ VarInRangeVar var1 var2
  Dead surfaceData var -> pure $ Dead surfaceData var
-------------------------------------------------------------------------------
noSelfReferencesRInt :: RInt -> TC RInt
noSelfReferencesRInt rInt =
  case rInt of
    RConstant surfaceData n -> pure rInt
    RThoughts surfaceData side -> pure rInt
    RKnowledge surfaceData knowledge side -> pure rInt
    RSelfProjection surfaceData lStat ->
     TC $
     Left
      ["Invalid reference to self in subexpression:\n" ++
       (getSurfaceSyntax surfaceData) ++
       (getLocationMessage surfaceData)]
    RVarProjection surfaceData rStat variable -> pure rInt
    RSum surfaceData firstRInt secondRInt ->
     joinTC $
     pure $
     RSum surfaceData
     <$> noSelfReferencesRInt firstRInt
     <*> noSelfReferencesRInt secondRInt
    RDifference surfaceData firstRInt secondRInt ->
     joinTC $
     pure $
     RDifference surfaceData
     <$> noSelfReferencesRInt firstRInt
     <*> noSelfReferencesRInt secondRInt
    RProduct surfaceData firstRInt secondRInt ->
     joinTC $
     pure $
     RProduct surfaceData
     <$> noSelfReferencesRInt firstRInt
     <*> noSelfReferencesRInt secondRInt
    RQuotient surfaceData firstRInt secondRInt ->
     joinTC $
     pure $
     RQuotient surfaceData
     <$> noSelfReferencesRInt firstRInt
     <*> noSelfReferencesRInt secondRInt
    RMod surfaceData firstRInt secondRInt ->
     joinTC $
     pure $
     RMod surfaceData
     <$> noSelfReferencesRInt firstRInt
     <*> noSelfReferencesRInt secondRInt
    RCardinality surfaceData var set condition ->
     joinTC $
     pure $
     RCardinality surfaceData var set
     <$> noSelfReferencesRBool condition
-------------------------------------------------------------------------------
noSelfReferencesCost :: Maybe RInt -> TC (Maybe RInt)
noSelfReferencesCost maybeCost =
 case maybeCost of
  Nothing -> pure Nothing
  Just cost ->
   fmap Just $
   noSelfReferencesRInt cost
-------------------------------------------------------------------------------
noSelfReferencesCondition :: Maybe RBool -> TC (Maybe RBool)
noSelfReferencesCondition maybeCondition =
 case maybeCondition of
  Nothing -> pure Nothing -- Don't I have a trivial condition or something?
  Just condition ->
   fmap Just $
   noSelfReferencesRBool condition
-------------------------------------------------------------------------------
noSelfReferencesLExpr :: LExpr -> TC LExpr
noSelfReferencesLExpr lExpr =
 case lExpr of
  LThoughtsExpr surfaceData side -> pure lExpr
  LKnowledgeExpr surfaceData knowledge side -> pure lExpr
  LSelfProjection surfaceData lStat ->
   TC $
   Left
    ["Invalid reference to self in subexpression:\n"
     ++ (getSurfaceSyntax surfaceData)
     ++ (getLocationMessage surfaceData)]
  LVarProjection surfaceData variable lStat -> pure lExpr
-------------------------------------------------------------------------------
noSelfReferencesAssignment :: Assignment -> TC Assignment
noSelfReferencesAssignment (Assignment lExprs mutator rInt) =
 joinTC $
 pure $
 Assignment
 <$> traverse noSelfReferencesLExpr lExprs
 <*> pure mutator
 <*> noSelfReferencesRInt rInt
-------------------------------------------------------------------------------
noSelfReferencesSkillEffect :: SkillEffect -> TC SkillEffect
noSelfReferencesSkillEffect skillEffect =
 case skillEffect of
  SkillEffectAssignment assignment ->
   joinTC $
   pure $
   SkillEffectAssignment
   <$> noSelfReferencesAssignment assignment
  SkillEffectDamageSelf surfaceData damage ->
   joinTC $
   pure $
   SkillEffectDamageSelf surfaceData
   <$> noSelfReferencesRInt damage
  SkillEffectDamageVar surfaceData var damage ->
   joinTC $
   pure $
   SkillEffectDamageVar surfaceData var
   <$> noSelfReferencesRInt damage
  SendVarToGraveyard surfaceData var ->
   pure $
   SendVarToGraveyard surfaceData var
  SendSelfToGraveyard surfaceData ->
    TC $
    Left
    ["Invalid reference to self in subexpression:\n"
     ++ (getSurfaceSyntax surfaceData)
     ++ (getLocationMessage surfaceData)]
  SkillEffectRevive surfaceData var ->
   pure $ SkillEffectRevive surfaceData var

{- | SkillEffectDamageSelf Lexer.Surface RInt
 | SkillEffectDamageVar Lexer.SurfaceData String RInt
SendVarToGraveyard : send var to graveyard {SendVarToGraveyard dummySurfaceData $2}
SendSelfToGraveyard : send self to graveyard {SendSelfToGraveyard dummySurfaceData}


-}

-- NEED TO ADD REVIVE CASE
-------------------------------------------------------------------------------
noSelfReferencesSkillEffects :: [SkillEffect] -> TC [SkillEffect]
noSelfReferencesSkillEffects skillEffects =
 traverse noSelfReferencesSkillEffect skillEffects
-------------------------------------------------------------------------------
noSelfReferencesAutomatic :: Automatic -> TC Automatic
noSelfReferencesAutomatic (Automatic surfaceData skillEffects nonAutomatic) =
 joinTC $
 pure $
 Automatic surfaceData
 <$> {-noSelfReferencesSkillEffects skillEffects-} undefined
 <*> noSelfReferencesNonautomatic nonAutomatic
noSelfReferencesAutomatic (Universal surfaceData judgment condition skillEffects nonautomatic) =
 joinTC $
 pure $
 Universal surfaceData judgment
 <$> noSelfReferencesRBool condition
 <*> {-traverse noSelfReferencesSkillEffect skillEffects-} undefined
 <*> noSelfReferencesNonautomatic nonautomatic
-------------------------------------------------------------------------------
noSelfReferencesNonautomatic :: Nonautomatic -> TC Nonautomatic
noSelfReferencesNonautomatic nonAutomatic =
 case nonAutomatic of
  Selection surfaceData judgments maybeCondition thenCase ifUnableCase nextAutomatic ->
   joinTC $
   pure $
   Selection surfaceData judgments
   <$> noSelfReferencesCondition maybeCondition
   <*> noSelfReferencesAutomatic thenCase
   <*> noSelfReferencesAutomatic ifUnableCase
   <*> noSelfReferencesAutomatic nextAutomatic
  TerminatedSkillComponent -> pure nonAutomatic
  Next automatic -> Next <$> noSelfReferencesAutomatic automatic
-------------------------------------------------------------------------------
noSelfReferencesSkill :: Skill -> TC Skill
noSelfReferencesSkill skill =
 case skill of
  Skill surfaceData cost condition automatic ->
   Skill surfaceData
    <$> noSelfReferencesCost cost
    <*> noSelfReferencesCondition condition
    <*> noSelfReferencesAutomatic automatic
-------------------------------------------------------------------------------
noSelfReferences :: (Skill -> a) -> Skill -> TC a
noSelfReferences f skill =
 f <$> noSelfReferencesSkill skill
-------------------------------------------------------------------------------
typeCheckSoul :: ParseTree.CarryingSource ParseTree.Soul -> TC Soul
typeCheckSoul (ParseTree.CarryingSource surfaceData (ParseTree.Soul skill)) =
 joinTC $
 noSelfReferences (Soul surfaceData)
 <$> typeCheckSkill skill
-------------------------------------------------------------------------------
{-level, etc, should be an arbitrary string in parsing... but a number after type checking..-}
typeCheckBaseLevel :: Lexer.SurfaceData -> TC BaseLevel
typeCheckBaseLevel (Lexer.SurfaceData row column surface) = 
 BaseLevel (Lexer.SurfaceData row column surface)
 <$> (typeCheckInt surface "Base level" 1 9)
-------------------------------------------------------------------------------
typeCheckBaseHp :: Lexer.SurfaceData -> TC BaseHp
typeCheckBaseHp (Lexer.SurfaceData row column surface) =
 BaseHp (Lexer.SurfaceData row column surface)
 <$> (typeCheckInt surface "Base hp" 1 1000) 
-------------------------------------------------------------------------------
typeCheckBaseAttack :: SurfaceData -> TC BaseAttack
typeCheckBaseAttack (Lexer.SurfaceData row column surface) =
 BaseAttack (Lexer.SurfaceData row column surface)
 <$> (typeCheckInt surface "Base attack" 0 1000)
-------------------------------------------------------------------------------
typeCheckInt :: String -> String -> Int -> Int -> TC Int
typeCheckInt s name lowerBound upperBound =
 case (readMaybe s :: Maybe Int) of
  Nothing -> TC $ Left [name ++ " must be an integer"]
  Just i ->
   if i < lowerBound
   then
    putErr $
    name ++
    " must be at least " ++
    (show lowerBound)
  else if i > upperBound
   then
    putErr $
    name ++
    " cannot exceed " ++
    (show upperBound)
   else pure i
-------------------------------------------------------------------------------
typeCheckConstant :: Lexer.SurfaceData -> String -> TC Int
typeCheckConstant (Lexer.SurfaceData row column surfaceSyntax) s =
 typeCheckInt
  s
  ("at position " ++
   (show row) ++
   "," ++
   (show column) ++
   ", " ++
   "constant expression" ++
   " " ++
   surfaceSyntax)
  minInt
  maxInt
-------------------------------------------------------------------------------
typeCheckLInt :: Context -> ParseTree.CarryingSource ParseTree.Expr -> TC LExpr {-and apparently these are all ints...-}
typeCheckLInt context (ParseTree.CarryingSource surfaceData expr) =
 case expr of
  ParseTree.Constant value ->
   TC $ Left ["cannot assign to constant"]
  ParseTree.ThoughtsExpr side -> pure $ LThoughtsExpr surfaceData side
  ParseTree.KnowledgeExpr knowledge side ->
   LKnowledgeExpr surfaceData
   <$> typeCheckKnowledge knowledge
   <*> pure side
  ParseTree.Self field ->
   LSelfProjection surfaceData <$> typeCheckRStatSelf field
  ParseTree.Var field variable ->
   LVarProjection surfaceData
   <$> typeCheckVariable context (Variable surfaceData variable)
   <*> typeCheckRStatSelf field
  ParseTree.Sum expr1 expr2 ->
   TC $ Left ["cannot assign to sum result"]
  ParseTree.Difference expr1 expr2 ->
   TC $ Left ["cannot assign to difference result"]
  ParseTree.Product expr1 expr2 ->
   TC $ Left ["cannot assign to product result"]
  ParseTree.Quotient expr1 expr2 ->
   TC $ Left ["cannot assign to quotient result"]
  ParseTree.Mod expr1 expr2 ->
   TC $ Left ["cannot assign to modulus result"]
  ParseTree.Always -> error "always should not exist, much less as an integer"
  ParseTree.GT expr1 expr2 ->
   TC $ Left ["cannot assign to inequality check"]
  ParseTree.GEQ expr1 expr2 ->
    TC $ Left ["cannot assign to inequality check"]
  ParseTree.LT expr1 expr2 ->
   TC $ Left ["cannot assign to inequality check"]
  ParseTree.LEQ expr1 expr2 ->
   TC $ Left ["cannot assign to inequality check"]
  ParseTree.EQ expr1 expr2 ->
   TC $ Left ["cannot assign to equality check"]
  ParseTree.And expr1 expr2 ->
   TC $ Left ["cannot assign to and result"]
  ParseTree.Or expr1 expr2 ->
   TC $ Left ["cannot assign to or result"]
  ParseTree.Not expr ->
   TC $ Left ["cannot assign to not result"]
-------------------------------------------------------------------------------
typeCheckRInt :: Context -> ParseTree.CarryingSource ParseTree.Expr -> TC RInt
typeCheckRInt context (ParseTree.CarryingSource surfaceData expr) = {-error "typeCheckRInt not implemented"-}
 case expr of
  ParseTree.Constant value -> RConstant surfaceData <$> typeCheckConstant surfaceData value
  ParseTree.ThoughtsExpr side -> pure $ RThoughts surfaceData side
  ParseTree.KnowledgeExpr knowledge side -> 
   RKnowledge surfaceData
   <$> typeCheckKnowledge knowledge
   <*> pure side
  ParseTree.Self field ->
   RSelfProjection surfaceData
   <$> typeCheckRStatSelf field
  ParseTree.Var field variable ->
   RVarProjection surfaceData
   <$> typeCheckRStatVar field
   <*> typeCheckVariable context (Variable surfaceData variable)
  ParseTree.Sum expr1 expr2 ->
   RSum surfaceData
   <$> typeCheckRInt context expr1
   <*> typeCheckRInt context expr2
  ParseTree.Difference expr1 expr2 ->
   RDifference surfaceData
   <$> typeCheckRInt context expr1
   <*> typeCheckRInt context expr2
  ParseTree.Product expr1 expr2 ->
   RProduct surfaceData
   <$> typeCheckRInt context expr1
   <*> typeCheckRInt context expr2
  ParseTree.Quotient expr1 expr2 ->
   RQuotient surfaceData
   <$> typeCheckRInt context expr1
   <*> typeCheckRInt context expr2
  ParseTree.Mod expr1 expr2 ->
   RMod surfaceData
   <$> typeCheckRInt context expr1
   <*> typeCheckRInt context expr2

{-PARSE TREE ALWAYS STILL EXISTS!??!?!?!?!?!?!?!??!?!?!?!?!?-}

  ParseTree.Always -> error "always should not exist, much less as an integer"
  ParseTree.GT expr1 expr2 ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "(>)"]
  ParseTree.GEQ expr1 expr2 ->
    TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "(>=)"]
  ParseTree.LT expr1 expr2 ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "(<)"]
  ParseTree.LEQ expr1 expr2 ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "(<=)"]
  ParseTree.EQ expr1 expr2 ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "(=)"]
  ParseTree.And expr1 expr2 ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "and"]
  ParseTree.Or expr1 expr2 ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "or"]
  ParseTree.Not expr ->
   TC $ Left [typeMismatchMessage surfaceData "Integer" "Boolean" "not"]
  ParseTree.Cardinality var set expr ->
   joinTC $
   pure $
   RCardinality surfaceData var set -- don't worry about correctness of var again for now... (also don't check or extend? context for now)
   <$> (joinTC $ typeCheckRBool <$> context' <*> pure expr)
   where binding = (var, set)
         judgment = mkJudgment binding
         context' = tryExtendContext context judgment

-- | RCardinality SurfaceData String ParseTree.Set RBool

-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------
{-Again... need to pass row and column to typecheckint... can actually pass entire surface syntax to it...-}
typeCheckBaseDefense :: SurfaceData -> TC BaseDefense
typeCheckBaseDefense (Lexer.SurfaceData row column surface) =
 BaseDefense (Lexer.SurfaceData row column surface)
 <$> (typeCheckInt surface "Base defense" 0 1000)
-------------------------------------------------------------------------------
typeCheckBaseSpeed :: SurfaceData -> TC BaseSpeed
typeCheckBaseSpeed (Lexer.SurfaceData row column surface) =
 BaseSpeed (Lexer.SurfaceData row column surface)
 <$> (typeCheckInt surface "Base speed" 1 5)
-------------------------------------------------------------------------------
typeCheckBaseRange :: SurfaceData -> TC BaseRange
typeCheckBaseRange (Lexer.SurfaceData row column surface) =
 BaseRange (Lexer.SurfaceData row column surface)
 <$> (typeCheckInt surface "Base range" 1 5)
-------------------------------------------------------------------------------
typeCheckBaseSoulPoints :: SurfaceData -> TC BaseSoulPoints
typeCheckBaseSoulPoints (Lexer.SurfaceData row column surface) =
 BaseSoulPoints (Lexer.SurfaceData row column surface)
 <$> (typeCheckInt surface "Base soul points" 1 2)
-------------------------------------------------------------------------------
typeCheckStats :: ParseTree.CarryingSource ParseTree.Stats -> TC Stats
typeCheckStats (ParseTree.CarryingSource _ (ParseTree.Stats surfaceData schools level hp attack defense speed range soulPoints)) =
 Stats surfaceData
 <$> typeCheckSchools schools
 <*> typeCheckBaseLevel level
 <*> typeCheckBaseHp hp
 <*> typeCheckBaseAttack attack
 <*> typeCheckBaseDefense defense
 <*> typeCheckBaseSpeed speed
 <*> typeCheckBaseRange range
 <*> typeCheckBaseSoulPoints soulPoints
-------------------------------------------------------------------------------
typeCheckSpawnSpell :: ParseTree.CarryingSource ParseTree.Skill -> TC Skill
typeCheckSpawnSpell skill =
-- There are other issues like can't send to field but for now just check against self reference
-- Actually for now I'm struggling to check against self reference so just don't for now.
 typeCheckSkill skill

{-

data Spell
 = Spell SurfaceData String Knowledge BaseLevel Skill {- name, school, level, skill -}


 joinTC $
 noSelfReferences (Soul surfaceData)
 <$> typeCheckSkill skill

-}

-------------------------------------------------------------------------------
typeCheckSpell :: ParseTree.Spell -> TC Spell
typeCheckSpell (ParseTree.Spell surfaceData name (ParseTree.Knowledge surfaceDataSchool) level skill) =
 Spell surfaceData name
 <$> typeCheckSchool surfaceDataSchool
 <*> typeCheckBaseLevel level
 <*> typeCheckSpawnSpell skill

{-
(joinTC $
   noSelfReferences (SpawnUnit surfaceData)
   <$> typeCheckSkill skill)
-}

-------------------------------------------------------------------------------
typeCheckUnit :: ParseTree.Unit -> TC Unit
typeCheckUnit (ParseTree.Unit name stats start end counter spawn death auto actions soul) =
 Unit name
 <$> typeCheckStats stats
 <*> typeCheckStart start 
 <*> typeCheckEnd end
 <*> typeCheckCounter counter
 <*> typeCheckSpawnUnit spawn
 <*> typeCheckDeath death
 <*> typeCheckAuto auto
 <*> typeCheckActions actions 
 <*> typeCheckSoul soul
-------------------------------------------------------------------------------
typeCheckUnits :: [ParseTree.Unit] -> TC [Unit]
typeCheckUnits = traverse typeCheckUnit
-------------------------------------------------------------------------------
typeCheckSpells :: [ParseTree.Spell] -> TC [Spell]
typeCheckSpells = traverse typeCheckSpell
-------------------------------------------------------------------------------
assumeFailure :: Either [String] a -> [String]
assumeFailure (Left s) = s
assumeFailure (Right _) = []
-------------------------------------------------------------------------------
typeCheck :: ParseTree.File -> TC File
typeCheck (ParseTree.File units spells) =
 --trace (show units) $
 File
 <$> (typeCheckUnits units)
 <*> (typeCheckSpells spells)
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- CUSTOM APPLICATIVE FUNCTOR FOR COLLECTING ERRORS
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------       
newtype TC a = TC {runTC :: Either [String] a} deriving Functor
-------------------------------------------------------------------------------
joinTC :: TC (TC a) -> TC a
joinTC = TC . join . fmap runTC . runTC
-------------------------------------------------------------------------------
getErr (TC (Right _)) = []
getErr (TC (Left x)) = x
-------------------------------------------------------------------------------
putErr :: String -> TC a
putErr = TC . Left . pure
-------------------------------------------------------------------------------
putErrs :: [String] -> TC a
putErrs = TC . Left
-------------------------------------------------------------------------------
instance Applicative TC where
 pure = TC . Right
 (TC (Right f)) <*> (TC (Right x)) = TC . Right . f $ x
 m <*> n = TC . Left $ (getErr m) <> (getErr n)
-------------------------------------------------------------------------------
