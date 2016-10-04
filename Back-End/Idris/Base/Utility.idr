module Base.Utility

import Data.Primitives.Views


export
div2 : Integer -> Integer
div2 dividend with (divides dividend 2)
  div2 ((2 * quotient) + remainder) | (DivBy prf) = quotient

div3 : Integer -> Integer
div3 dividend with (divides dividend 3)
  div3 ((3 * quotient) + remainder) | (DivBy prf) = quotient

export
div4 : Integer -> Integer
div4 dividend with (divides dividend 4)
  div4 ((4 * quotient) + remainder) | (DivBy prf) = quotient

export
mod2 : Integer -> Integer
mod2 dividend with (divides dividend 2)
  mod2 ((2 * quotient) + remainder) | (DivBy prf) = remainder

export
mod3 : Integer -> Integer
mod3 dividend with (divides dividend 3)
  mod3 ((3 * quotient) + remainder) | (DivBy prf) = remainder

export
mod6 : Integer -> Integer
mod6 dividend with (divides dividend 6)
  mod6 ((6 * quotient) + remainder) | (DivBy prf) = remainder

