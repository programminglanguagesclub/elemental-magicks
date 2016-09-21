module skill_dsl_syntax2
import Data.Vect
import Data.Fin
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import hp
import preliminaries
import objects_basic

import card {-STILL HAVE TO IMPORT THIS IN SYNTAX1-}
import player
import skill_dsl_data
%access public export
%default total


namespace AutomaticFactory
  done : AutomaticFactory
  done = MkAutomaticFactory [] TerminatedSkillFactory

namespace NonautomaticFactory
  done : NonautomaticFactory
  done = TerminatedSkillFactory

{-for now, in the DSL, I am not supporting syntax for unions. This will come later!-}


{-I should maybe just be defining everything in terms of these two datatypes... for now let me just use this to marshall to Set. Maybe later things will be changed-}
data Side = Friendly | Enemy
data RelativeSet = RelativeBoard | RelativeSpawn | RelativeHand | RelativeGraveyard | RelativeDiscard
getSet : Side -> RelativeSet -> Set


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


{-
permanent : Temporality
permanent = Permanent

temporary : Temporality
temporary = Temporary
-}


namespace statL
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
{-
namespace statR
  range : StatR
  range = RangeR
  speed : StatR
  speed = SpeedR
  defense : StatR
  defense = DefenseR
  attack : StatR
  attack = AttackR
  level : StatR
  level = LevelR
  hp : StatR
  maxHp : StatR
-}


x : String
x = "x"
y : String
y = "y"
z : String
z = "z"





{-






scouting : Skill
scouting = soul;
 all monster x in enemy board
   permanent range x -= 1


-}


namespace skipAutomatic
  begin : NonautomaticFactory -> AutomaticFactory
  begin NonautomaticFactory = MkAutomaticFactory [] NonautomaticFactory
namespace doNotSkipAutomatic
  begin : NonautomaticFactory -> NonautomaticFactory
  begin = \x => x
namespace skill_start
  begin : NonautomaticFactory -> Nat -> SkillFactory
  begin NonautomaticFactory cost = (MkAutomaticFactory [] NonautomaticFactory, False, cost)
namespace skill_start_list
  begin : NonautomaticFactory -> Nat -> List SkillFactory
  begin NonautomaticFactory cost = [(MkAutomaticFactory [] NonautomaticFactory, False, cost)]


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




{-
syntax exists [var] "in" [side] [relativeSet] success ":" [sel] "_" = (ExistentialFactory [(var, getSet side relativeSet)] Vacuous sel done)

syntax exists [var] "in" [side] [relativeSet] "where" [cond] success ":" [sel] "_" = begin (ExistentialFactory [(var, getSet side relativeSet)] cond sel done)
-}

{--}
replaceSuccess : AutomaticFactory -> NonautomaticFactory -> NonautomaticFactory
replaceSuccess success (ExistentialFactory args cond sel fail) = ExistentialFactory args cond success fail
replaceSuccess _ TerminatedSkillFactory = TerminatedSkillFactory
replaceFailure : AutomaticFactory -> NonautomaticFactory -> NonautomaticFactory
replaceFailure failure (ExistentialFactory args cond sel fail) = ExistentialFactory args cond sel failure
replaceFailure _ TerminatedSkillFactory = TerminatedSkillFactory

{-
syntax success ":" [sel] =
-}














{-for now just have single variable selections. I'll allow multiple very soon -}
{-
syntax select [var] "in" [side] [relativeSet] "where" [cond] "then" "{" [sel] "}" "else" "{" [fail] "}" next "{" [next] "}" = 4
-}



namespace storeAutomatic
 storeSkill : AutomaticFactory -> AutomaticFactory
 storeSkill = id
namespace storeNonautomaticFactory
 storeSkill : NonautomaticFactory -> AutomaticFactory
 storeSkill NonautomaticFactory = MkAutomaticFactory [] NonautomaticFactory

{-
data ThenStatement = MkThenStatement AutomaticFactory
data ElseStatement = MkElseStatement AutomaticFactory
data NextStatement = MkNextStatement AutomaticFactory         
-}
{-
syntax "then" "{" [sel] "}" = MkThenStatement (storeSkill sel)
syntax "else" "{" [fail] "}" = MkElseStatement (storeSkill fail)
syntax next "{" [next] "}" = MkNextStatement (storeSkill next)
-}


{-For now single argument, not argument list-}
{-
namespace statement_then_else_next_NonautomaticFactory
 addStatements : (String,Side,RelativeSet) -> ThenStatement -> ElseStatement -> NextStatement -> NonautomaticFactory
 addStatements = ?hole
namespace statement_then_else_NonautomaticFactory
 addStatements : (String,Side,RelativeSet) -> ThenStatement -> ElseStatement -> NonautomaticFactory
 addStatements = ?hole
namespace statement_then_next_NonautomaticFactory
 addStatements : (String,Side,RelativeSet) -> ThenStatement -> NextStatement -> NonautomaticFactory
 addStatements = ?hole
namespace statement_else_next_NonautomaticFactory
 addStatements : (String,Side,RelativeSet) -> ElseStatement -> NextStatement -> NonautomaticFactory
 addStatements = ?hole
namespace statement_then_NonautomaticFactory
 addStatements : (String,Side,RelativeSet) -> ThenStatement -> NonautomaticFactory
 addStatements = ?hole
namespace statement_else_NonautomaticFactory
 addStatements : (String,Side,RelativeSet) -> ElseStatement -> NonautomaticFactory
 addStatements = ?hole





namespace statement_then_else_next_automatic
 addStatements : (String,Side,RelativeSet) -> ThenStatement -> ElseStatement -> NextStatement -> AutomaticFactory
 addStatements = ?hole
namespace statement_then_else_automatic
 addStatements : (String,Side,RelativeSet) -> ThenStatement -> ElseStatement -> AutomaticFactory
 addStatements = ?hole
namespace statement_then_next_automatic
 addStatements : (String,Side,RelativeSet) -> ThenStatement -> NextStatement -> AutomaticFactory
 addStatements = ?hole
namespace statement_else_next_automatic
 addStatements : (String,Side,RelativeSet) -> ElseStatement -> NextStatement -> AutomaticFactory
 addStatements = ?hole
namespace statement_then_automatic
 addStatements : (String,Side,RelativeSet) -> ThenStatement -> AutomaticFactory
 addStatements = ?hole
namespace statement_else_automatic
 addStatements : (String,Side,RelativeSet) -> ElseStatement -> AutomaticFactory
 addStatements = ?hole



-}

{-
namespace auto_nonauto
namespace auto_auto
 castSkill : AutomaticFactory -> AutomaticFactory
 castSkill = id
namespace nonauto_auto

namespace nonauto_nonauto
 castSkill : NonautomaticFactory -> NonautomaticFactory
 castSkill = id


-}















































{-

syntax select [var] "in" [side] [relativeSet] = addStatements (var,side,relativeSet)
-}

{-need to add more arguments to represent the data in var, side, and relative set-}





{-
foo : Integer
foo = select x in friendly board where Vacuous then { done } else { done } next { done }
                                       -}
{-

foo : thenStatement
foo = then {hp x := 3}
-}


















{-
syntax hp [var] [mutator] [val] = SkillEffectStatEffect (MkHpEffect mutator CurrentHp val) var
syntax maxHp [var] [mutator] [val] = SkillEffectStatEffect (MkHpEffect mutator MaxHp val) var
-}





namespace getInteger
  hp : String -> RInteger
  hp var = Variable HpR var
  maxHp : String -> RInteger
  maxHp var = Variable MaxHpR var
  
{-eventually might want to use Stat for this, but then need it to not bake in temporality-}


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



{-
syntax ";" [effect] [effects] = effect::effects
syntax "do" [effect] [effects] = effect::effects
-}

{-
foo : NonautomaticFactory
foo = exists x in friendly board where Vacuous success : done failure : done

foo2 : AutomaticFactory
foo2 = all x in friendly board where Vacuous do
  []
  done



-}

infixr 4 += {-shouuld not have to write the fixity and associativity and precedence of constants!!-}
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
  



{-
syntax "(-=)" [val] = Increment (val * (-1))
-}

{-
(-=) : Integer -> Mutator
(-=) x = 
  -}

syntax "-=" = (-=)
syntax "+=" = (+=)
syntax ":=" = (:=)





{-

THEREFORE ALSO REDOING THIS


healing_rain : AutomaticFactory
healing_rain =
  all x in friendly board do
    [hp x (+=) (Constant 40)]
    done

scouting : AutomaticFactory
scouting =
  all x in enemy board do
    [permanent range x -= (Constant 1)]
    done

eye_of_clairvoyance : AutomaticFactory
eye_of_clairvoyance =
  all x in enemy board do
    [permanent speed x -= (Constant 1)]
    done





-}



{-test-}

namespace constant_constant
{-syntax [x] "<" [y] = LT (Constant x) (Constant y)-}
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



bar : Condition
bar = 32 < 40

baz : Condition
baz = (ThoughtsR True) < (ThoughtsR False)




{-Still have to cover the existence of spell cards and empty squares that could be selected. For now ignore this.-}


{-
syntax exists friendly unit [x] success ":" [sel] failure ":" [fail] = ExistentialFactory [] Vacuous sel fail
syntax exists enemy unit [x] success ":" [sel] failure ":" [fail] = ExistentialFactory [] Vacuous sel fail


foo : NonautomaticFactory
foo = exists friendly unit "x" success : done failure : done

-}
{-
foo : thenStatement
foo = then { (hp x := 3) }-}

{-

foo : ThenStatement
foo = then { MkAutomaticFactory [] done }

bar44 : ThenStatement -> NonautomaticFactory
bar44 = select x in friendly board

foobar44 : NonautomaticFactory
foobar44 = bar44 foo

{-GOING TO HAVE TO REDO THIS BECAUSE I CAN'T WRITE THE BELOW WITHOUT PARENS-}

foobar22 : NonautomaticFactory
foobar22 = (select x in friendly board) (then {MkAutomaticFactory [] done})

-}











{-


This works, but it has semicolons to make the syntax extension prefix-free.

-}





{-
REDOING THIS!!!

syntax select [var] "in" [side] [relativeSet] "where" [cond] "then" [sel] failure ":" [fail] = begin (ExistentialFactory [(var,getSet side relativeSet)] cond (finishWith sel) (finishWith fail))
syntax select [var] "in" [side] [relativeSet] "where" [cond] failure ":" [fail] = begin (ExistentialFactory [(var,getSet side relativeSet)] cond done (finishWith fail))
syntax select [var] "in" [side] [relativeSet] "where" [cond] "then" [sel] ";" = begin (ExistentialFactory [(var,getSet side relativeSet)] cond (finishWith sel) done)
syntax select [var] "in" [side] [relativeSet] "then" [sel] failure ":" [fail] = begin (ExistentialFactory [(var,getSet side relativeSet)] Vacuous sel (finishWith fail))
syntax select [var] "in" [side] [relativeSet] failure ":" [fail] = begin (ExistentialFactory [(var,getSet side relativeSet)] Vacuous done (finishWith fail))
syntax select [var] "in" [side] [relativeSet] "then" [sel] ";" = begin (ExistentialFactory [(var,getSet side relativeSet)] Vacuous (finishWith sel) done)
syntax all [var] "in" [side] [relativeSet] "where" [cond] "do" [effects] [next] = Universal (var, getSet side relativeSet) cond (eff effects) next
syntax all [var] "in" [side] [relativeSet] "do" [effects] [next] = Universal (var, getSet side relativeSet) Vacuous (eff effects) next
-}


{-if I have to use semicolons, then i can just require writing a next and use done for that.-}
syntax select [var] "in" [side] [relativeSet] "then" "{" [thenSkill] "}" ";" = begin (ExistentialFactory [(var, getSet side relativeSet)] Vacuous (finishWith thenSkill) done) 
syntax select [var] "in" [side] [relativeSet] "else" "{" [elseSkill] "}" ";" = begin (ExistentialFactory [(var, getSet side relativeSet)] Vacuous done (finishWith elseSkill)) 
syntax select [var] "in" [side] [relativeSet] "then" "{" [thenSkill] "}" "else" "{" [elseSkill] "}" ";" = begin (ExistentialFactory [(var, getSet side relativeSet)] Vacuous (finishWith thenSkill) (finishWith elseSkill))
syntax select [var] "in" [side] [relativeSet] "then" "{" [thenSkill] "}" "next" "{" [nextSkill] "}" ";" = ?hole
syntax select [var] "in" [side] [relativeSet] "else" "{" [thenSkill] "}" "next" "{" [nextSkill] "}" ";" = ?hole
syntax select [var] "in" [side] [relativeSet] "then" "{" [thenSkill] "}" "else" "{" [elseSkill] "}" "next" "{" [nextSkill] "}" ";" = ?hole

{-also have to have the versions with "where"-}


{-this also needs to be wrapped in something like begin or finishWith so it can be either AutomaticFactory or not..-}
syntax every [var] "in" [side] [relativeSet] [effects] [next] = Universal (var, getSet side relativeSet) Vacuous effects next {-single effects might not be simple, so I might require brackets always..-} 
{-every is another place where I would like to be able to leave off the done at the end...-}

syntax gaa [a] {-faa [b]-} = MkAutomaticFactory a done {-a b-}



garg : AutomaticFactory
garg = gaa [hp x := 0] {-faa (select x in enemy board then [hp x := 0] ;)-}


{- ah, what goes in the then cause right now can't just be a list of effects, but rather a whole AutomaticFactory skill.
I should allow skill effect list (with namespaces.. think I actually did this...) to work....
-}

{-garg = gaa [friendly thoughts := 0] faa (select x in enemy board then [hp x := 0] ;)-}



foobarbarbar : AutomaticFactory
foobarbarbar = every x in friendly board [hp x := 1] done

foo : AutomaticFactory
foo = select x in friendly board then { MkAutomaticFactory [] select y in friendly board then {MkAutomaticFactory [] done};};

barfoobar : AutomaticFactory
barfoobar = select x in friendly board then {MkAutomaticFactory [] done } else { MkAutomaticFactory [] done} ;





