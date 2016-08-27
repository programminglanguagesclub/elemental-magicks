module skill_dsl_syntax
import Data.Vect
import Data.Fin
import Data.So
import bounded
import bounded_then_integer
import integer_then_bounded
import hp
import preliminaries
import objects_basic
import skill_dsl_data
%access public export
%default total


namespace automatic
  done : Automatic
  done = MkAutomatic [] TerminatedSkill

namespace nonautomatic
  done : Nonautomatic
  done = TerminatedSkill

{-for now, in the DSL, I am not supporting syntax for unions. This will come later!-}


{-I should maybe just be defining everything in terms of these two datatypes... for now let me just use this to marshall to Set. Maybe later things will be changed-}
data Side = Friendly | Enemy
data RelativeSet = RelativeBoard | RelativeSpawn | RelativeHand | RelativeGraveyard | RelativeDiscard
getSet : Side -> RelativeSet -> Set


friendly : Side
friendly = Friendly

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



x : String
x = "x"
y : String
y = "y"
z : String
z = "z"




syntax exists [var] "in" [side] [relativeSet] "where" [cond] success ":" [sel] failure ":" [fail] = Existential [(var,getSet side relativeSet)] cond sel fail

syntax all [var] "in" [side] [relativeSet] "where" [cond] "do" [effects] [next] = Universal (var, getSet side relativeSet) cond effects next

{-
syntax hp [var] [mutator] [val] = SkillEffectStatEffect (MkHpEffect mutator CurrentHp val) var
syntax maxHp [var] [mutator] [val] = SkillEffectStatEffect (MkHpEffect mutator MaxHp val) var
-}

hp : String -> Mutator -> Integer -> SkillEffect
hp var mutator val = SkillEffectStatEffect (MkHpEffect mutator CurrentHp val) var
maxHp : String -> Mutator -> Integer -> SkillEffect
maxHp var mutator val = SkillEffectStatEffect (MkHpEffect mutator MaxHp val) var







foo : Nonautomatic
foo = exists x in friendly board where Vacuous success : done failure : done

foo2 : Automatic
foo2 = all x in friendly board where Vacuous do
  []
  done

infixr 4 += {-shouuld not have to write the fixity and associativity and precedence of constants!!-}
(+=) : Mutator
(+=) = Increment


healing_rain : Automatic
healing_rain =
  all x in friendly board where Vacuous do
    [hp x (+=) 40]
    done


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
syntax exists friendly unit [x] success ":" [sel] failure ":" [fail] = Existential [] Vacuous sel fail
syntax exists enemy unit [x] success ":" [sel] failure ":" [fail] = Existential [] Vacuous sel fail


foo : Nonautomatic
foo = exists friendly unit "x" success : done failure : done

-}


