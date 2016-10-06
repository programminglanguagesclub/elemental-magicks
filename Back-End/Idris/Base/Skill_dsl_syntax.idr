module Base.Skill_dsl_syntax
import Base.Card
import Base.Skill_dsl_data
%access public export
%default total


namespace AutomaticFactory
  done : AutomaticFactory
  done = MkAutomaticFactory [] TerminatedSkillFactory

namespace NonautomaticFactory
  done : NonautomaticFactory
  done = TerminatedSkillFactory

{-for now, in the DSL, I am not supporting syntax for unions. This will come later!-}


friendly : Side
friendly = Friendly
enemy : Side
enemy = Enemy
board : RelativeSet
board = RelativeBoard
spawn : RelativeSet
spawn = RelativeSpawn
hand : RelativeSet
hand = RelativeHand
graveyard : RelativeSet
graveyard = RelativeGraveyard
discard : RelativeSet
discard = RelativeDiscard
namespace stat
  range : Stat
  range = Range
  speed : Stat
  speed = Speed
  defense : Stat
  defense = Defense
  attack : Stat
  attack = Attack
  level : Stat
  level = Level
namespace variables
  x : String
  x = "x"
  y : String
  y = "y"
  z : String
  z = "z"
  w : String
  w = "w"
  a : String
  a = "a"
  b : String
  b = "b"
  c : String
  c = "c"
  d : String
  d = "d"
  e : String
  e = "e"


namespace skipAutomatic
  begin : NonautomaticFactory -> AutomaticFactory
  begin NonautomaticFactory = MkAutomaticFactory [] NonautomaticFactory
namespace doNotSkipAutomatic
  begin : NonautomaticFactory -> NonautomaticFactory
  begin = \x => x
namespace skill_start
  begin : NonautomaticFactory -> Nat -> SkillFactory
  begin NonautomaticFactory cost = (MkAutomaticFactory [] NonautomaticFactory, cost, Vacuous)
namespace skill_start_list
  begin : NonautomaticFactory -> Nat -> List SkillFactory
  begin NonautomaticFactory cost = [(MkAutomaticFactory [] NonautomaticFactory, cost, Vacuous)]


namespace finishWithList
  finishWith : List SkillEffect -> AutomaticFactory
  finishWith skillEffects = MkAutomaticFactory skillEffects done
namespace finishWithSingle
  finishWith : SkillEffect -> AutomaticFactory
  finishWith skillEffect = MkAutomaticFactory [skillEffect] done
namespace doNotFinishWith
  finishWith : AutomaticFactory -> AutomaticFactory
  finishWith = \x => x



namespace universalEffects
  eff : List SkillEffect -> List SkillEffect
  eff = id
namespace universalEffects2
  eff : SkillEffect -> List SkillEffect
  eff s = [s]

namespace storeAutomatic
 storeSkill : AutomaticFactory -> AutomaticFactory
 storeSkill = id
namespace storeNonautomaticFactory
 storeSkill : NonautomaticFactory -> AutomaticFactory
 storeSkill NonautomaticFactory = MkAutomaticFactory [] NonautomaticFactory

namespace getInteger
  hp : String -> RInteger
  hp var = Variable HpR var
  maxHp : String -> RInteger
  maxHp var = Variable MaxHpR var
  permanent : Stat -> String -> RInteger
  permanent Attack var = Variable PermanentAttackR var
  permanent Defense var = Variable PermanentDefenseR var
  permanent Speed var = Variable PermanentSpeedR var
  permanent Range var = Variable PermanentRangeR var
  permanent Level var = Variable PermanentLevelR var
  temporary : Stat -> String -> RInteger
  temporary Attack var = Variable TemporaryAttackR var
  temporary Defense var = Variable TemporaryDefenseR var
  temporary Speed var = Variable TemporarySpeedR var
  temporary Range var = Variable TemporaryRangeR var
  temporary Level var = Variable TemporaryLevelR var
namespace rinteger
  hp : String -> Mutator -> RInteger -> SkillEffect
  hp var mutator val = SkillEffectStatEffect (MkHpEffect mutator CurrentHp val) var
  maxHp : String -> Mutator -> RInteger -> SkillEffect
  maxHp var mutator val = SkillEffectStatEffect (MkHpEffect mutator MaxHp val) var
  temporary : Stat -> String -> Mutator -> RInteger -> SkillEffect
  temporary stat var mutator val = SkillEffectStatEffect (MkStatEffect stat mutator Temporary val) var
  permanent : Stat -> String -> Mutator -> RInteger -> SkillEffect
  permanent stat var mutator val = SkillEffectStatEffect (MkStatEffect stat mutator Permanent val) var
revive : String -> SkillEffect
revive var = SkillEffectStatEffect ReviveEffect var
namespace cinteger
  hp : String -> Mutator -> Integer -> SkillEffect
  hp var mutator val = SkillEffectStatEffect (MkHpEffect mutator CurrentHp (Constant val)) var
  maxHp : String -> Mutator -> Integer -> SkillEffect
  maxHp var mutator val = SkillEffectStatEffect (MkHpEffect mutator MaxHp (Constant val)) var
  permanent : Stat -> String -> Mutator -> Integer -> SkillEffect
  permanent stat var mutator val = SkillEffectStatEffect (MkStatEffect stat mutator Permanent (Constant val)) var
syntax not [cond] = Not cond
dead : String -> Condition
dead var = RDead var
infixr 4 +=
(+=) : Mutator
(+=) = Increment
infixr 4 -=
(-=) : Mutator
(-=) = Decrement
infixr 4 :=
(:=) : Mutator
(:=) = Assign
namespace RR
  infixl 9 *
  (*) : RInteger -> RInteger -> RInteger
  (*) a b = Mult a b
namespace RI
  infixl 9 *
  (*) : RInteger -> Integer -> RInteger
  (*) a b = Mult a (Constant b)
namespace IR
  infixl 9 *
  (*) : Integer -> RInteger -> RInteger
  (*) a b = Mult (Constant a) b
syntax "-=" = (-=)
syntax "+=" = (+=)
syntax ":=" = (:=)
namespace constant_constant
  (<) : Integer -> Integer -> Condition
  (<) x y = LT (Constant x) (Constant y)
  (<=) : Integer -> Integer -> Condition
  (<=) x y = LEQ (Constant x) (Constant y)
  (>) : Integer -> Integer -> Condition
  (>) x y = GT (Constant x) (Constant y)
  (>=) : Integer -> Integer -> Condition
  (>=) x y = GEQ (Constant x) (Constant y)
  (==) : Integer -> Integer -> Condition
  (==) x y = EQ (Constant x) (Constant y)
namespace constant_non_constant
  (<) : Integer -> RInteger -> Condition
  (<) x y = LT (Constant x) y
  (<=) : Integer -> RInteger -> Condition
  (<=) x y = LEQ (Constant x) y
  (>) : Integer -> RInteger -> Condition
  (>) x y = GT (Constant x) y
  (>=) : Integer -> RInteger -> Condition
  (>=) x y = GEQ (Constant x) y
  (==) : Integer -> RInteger -> Condition
  (==) x y = EQ (Constant x) y
namespace non_constant_constant
  (<) : RInteger -> Integer -> Condition
  (<) x y = LT x (Constant y)
  (<=) : RInteger -> Integer -> Condition
  (<=) x y = LEQ x (Constant y)
  (>) : RInteger -> Integer -> Condition
  (>) x y = GT x (Constant y)
  (>=) : RInteger -> Integer -> Condition
  (>=) x y = GEQ x (Constant y)
  (==) : RInteger -> Integer -> Condition
  (==) x y = EQ x (Constant y)
namespace non_constant_non_constant
  (<) : RInteger -> RInteger -> Condition
  (<) x y = LT x y
  (<=) : RInteger -> RInteger -> Condition
  (<=) x y = LEQ x y
  (>) : RInteger -> RInteger -> Condition
  (>) x y = GT x y
  (>=) : RInteger -> RInteger -> Condition
  (>=) x y = GEQ x y
  (==) : RInteger -> RInteger -> Condition
  (==) x y = EQ x y
{-Still have to cover the existence of spell cards and empty squares that could be selected. For now ignore this.-}
{-This works, but it has semicolons to make the syntax extension prefix-free.-}

syntax select [var] "in" [side] [relativeSet] "then" "{" [thenSkill] "}" ";" = begin (ExistentialFactory [(var, getSet side relativeSet)] Vacuous (finishWith thenSkill) done) 
syntax select [var] "in" [side] [relativeSet] "then" "{" [thenSkill] "}" "else" "{" [elseSkill] "}" ";" = begin (ExistentialFactory [(var, getSet side relativeSet)] Vacuous (finishWith thenSkill) (finishWith elseSkill))
syntax select [var] "in" [side] [relativeSet] "then" "{" [thenSkill] "}" "next" "{" [nextSkill] "}" ";" = ?hole
syntax select [var] "in" [side] [relativeSet] "then" "{" [thenSkill] "}" "else" "{" [elseSkill] "}" "next" "{" [nextSkill] "}" ";" = ?hole

syntax select [var] "in" [side] [relativeSet] "where" [cond] "then" "{" [thenSkill] "}" ";" = begin (ExistentialFactory [(var, getSet side relativeSet)] cond (finishWith thenSkill) done)
syntax select [var] "in" [side] [relativeSet] "where" [cond] "then" "{" [thenSkill] "}" "else" "{" [elseSkill] "}" ";" = begin (ExistentialFactory [(var, getSet side relativeSet)] cond (finishWith thenSkill) (finishWith elseSkill))
syntax select [var] "in" [side] [relativeSet] "where" [cond] "then" "{" [thenSkill] "}" "next" "{" [nextSkill] "}" ";" = ?hole
syntax select [var] "in" [side] [relativeSet] "where" [cond] "then" "{" [thenSkill] "}" "else" "{" [elseSkill] "}" "next" "{" [nextSkill] "}" ";" = ?hole


{-I will probably have to redo this when I add more strict rules to what goes in a condition (then I will probably want two separate conditions...). Same goes for the "every" case.-}
syntax "if" [if_cond] select [var] "in" [side] [relativeSet] "then" "{" [thenSkill] "}" ";" = begin (ExistentialFactory [(var, getSet side relativeSet)] if_cond (finishWith thenSkill) done)
syntax "if" [if_cond] select [var] "in" [side] [relativeSet] "then" "{" [thenSkill] "}" "else" "{" [elseSkill] "}" ";" = begin (ExistentialFactory [(var, getSet side relativeSet)] if_cond (finishWith thenSkill) (finishWith elseSkill))
syntax "if" [if_cond] select [var] "in" [side] [relativeSet] "then" "{" [thenSkill] "}" "next" "{" [nextSkill] "}" ";" = ?hole
syntax "if" [if_cond] select [var] "in" [side] [relativeSet] "then" "{" [thenSkill] "}" "else" "{" [elseSkill] "}" "next" "{" [nextSkill] "}" ";" = ?hole
     
syntax "if" [if_cond] select [var] "in" [side] [relativeSet] "where" [cond] "then" "{" [thenSkill] "}" ";" = begin (ExistentialFactory [(var, getSet side relativeSet)] (And cond if_cond) (finishWith thenSkill) done)
syntax "if" [if_cond] select [var] "in" [side] [relativeSet] "where" [cond] "then" "{" [thenSkill] "}" "else" "{" [elseSkill] "}" ";" = begin (ExistentialFactory [(var, getSet side relativeSet)] (And cond if_cond) (finishWith thenSkill) (finishWith elseSkill))
syntax "if" [if_cond] select [var] "in" [side] [relativeSet] "where" [cond] "then" "{" [thenSkill] "}" "next" "{" [nextSkill] "}" ";" = ?hole
syntax "if" [if_cond] select [var] "in" [side] [relativeSet] "where" [cond] "then" "{" [thenSkill] "}" "else" "{" [elseSkill] "}" "next" "{" [nextSkill] "}" ";" = ?hole


{-also have to have the versions with "where"-}
{-this also needs to be wrapped in something like begin or finishWith so it can be either AutomaticFactory or not..-}


{- THIS IS ALL MESSED UP!! -}

syntax every [var] "in" [side] [relativeSet] "do" [effects] [next] = UniversalFactory (var, getSet side relativeSet) Vacuous effects next {-single effects might not be simple, so I might require brackets always..-} 
syntax every [var] "in" [side] [relativeSet] "where" [cond] "do" [effects] "next" [nextSkill] = UniversalFactory (var, getSet side relativeSet) cond effects nextSkill

syntax "if" [if_cond] every [var] "in" [side] [relativeSet] "do" [effects] [nextSkill] = UniversalFactory (var, getSet side relativeSet) if_cond effects nextSkill {-single effects might not be simple, so I might require brackets always..-} 
syntax "if" [if_cond] every [var] "in" [side] [relativeSet] "where" [cond] "do" [effects] "next" [nextSkill] = UniversalFactory (var, getSet side relativeSet) (And cond if_cond)


