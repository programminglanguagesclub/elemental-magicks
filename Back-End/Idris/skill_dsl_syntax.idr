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
import player
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
  begin : Nonautomatic -> Automatic
  begin nonautomatic = MkAutomatic [] nonautomatic
namespace doNotSkipAutomatic
  begin : Nonautomatic -> Nonautomatic
  begin = \x => x
namespace skill_start
  begin : Nonautomatic -> Nat -> Skill
  begin nonautomatic cost = (MkAutomatic [] nonautomatic, False, cost)
namespace skill_start_list
  begin : Nonautomatic -> Nat -> List Skill
  begin nonautomatic cost = [(MkAutomatic [] nonautomatic, False, cost)]


namespace finishWithList
  finishWith : List SkillEffect -> Automatic
  finishWith skillEffects = MkAutomatic skillEffects done
namespace finishWithSingle
  finishWith : SkillEffect -> Automatic
  finishWith skillEffect = MkAutomatic [skillEffect] done
namespace doNotFinishWith
  finishWith : Automatic -> Automatic
  finishWith = \x => x



namespace universalEffects
  eff : List SkillEffect -> List SkillEffect
  eff = id
namespace universalEffects2
  eff : SkillEffect -> List SkillEffect
  eff s = [s]




{-
syntax exists [var] "in" [side] [relativeSet] success ":" [sel] "_" = (Existential [(var, getSet side relativeSet)] Vacuous sel done)

syntax exists [var] "in" [side] [relativeSet] "where" [cond] success ":" [sel] "_" = begin (Existential [(var, getSet side relativeSet)] cond sel done)
-}

{--}
replaceSuccess : Automatic -> Nonautomatic -> Nonautomatic
replaceSuccess success (Existential args cond sel fail) = Existential args cond success fail
replaceSuccess _ TerminatedSkill = TerminatedSkill
replaceFailure : Automatic -> Nonautomatic -> Nonautomatic
replaceFailure failure (Existential args cond sel fail) = Existential args cond sel failure
replaceFailure _ TerminatedSkill = TerminatedSkill

{-
syntax success ":" [sel] =
-}



syntax select [var] "in" [side] [relativeSet] "where" [cond] "then" [sel] failure ":" [fail] = begin (Existential [(var,getSet side relativeSet)] cond (finishWith sel) (finishWith fail))

syntax select [var] "in" [side] [relativeSet] "where" [cond] failure ":" [fail] = begin (Existential [(var,getSet side relativeSet)] cond done (finishWith fail))

syntax select [var] "in" [side] [relativeSet] "where" [cond] "then" [sel] ";" = begin (Existential [(var,getSet side relativeSet)] cond (finishWith sel) done)


syntax select [var] "in" [side] [relativeSet] "then" [sel] failure ":" [fail] = begin (Existential [(var,getSet side relativeSet)] Vacuous sel (finishWith fail))

syntax select [var] "in" [side] [relativeSet] failure ":" [fail] = begin (Existential [(var,getSet side relativeSet)] Vacuous done (finishWith fail))

syntax select [var] "in" [side] [relativeSet] "then" [sel] ";" = begin (Existential [(var,getSet side relativeSet)] Vacuous (finishWith sel) done)




syntax all [var] "in" [side] [relativeSet] "where" [cond] "do" [effects] [next] = Universal (var, getSet side relativeSet) cond (eff effects) next

syntax all [var] "in" [side] [relativeSet] "do" [effects] [next] = Universal (var, getSet side relativeSet) Vacuous (eff effects) next

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
foo : Nonautomatic
foo = exists x in friendly board where Vacuous success : done failure : done

foo2 : Automatic
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


healing_rain : Automatic
healing_rain =
  all x in friendly board do
    [hp x (+=) (Constant 40)]
    done

scouting : Automatic
scouting =
  all x in enemy board do
    [permanent range x -= (Constant 1)]
    done

eye_of_clairvoyance : Automatic
eye_of_clairvoyance =
  all x in enemy board do
    [permanent speed x -= (Constant 1)]
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


