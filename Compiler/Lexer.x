{

module Lexer where

import System.IO
}

%wrapper "monad"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters



tokens :-
<0>  $white+				;

<0> soulSkill { \s i -> return (SoulSkill, lineNumber s) }
<0> start { \s i -> return (StartSkill, lineNumber s) }
<0> end { \s i -> return (EndSkill, lineNumber s) }
<0> counter { \s i -> return (CounterSkill, lineNumber s) }
<0> spawn { \s i -> return (SpawnSkill, lineNumber s) }
<0> death { \s i -> return (DeathSkill, lineNumber s) }
<0> auto { \s i -> return (AutoSkill, lineNumber s) }
<0> action { \s i -> return (ActionSkill, lineNumber s) }
<0> union { \s i -> return (Union, lineNumber s) }


<0> unit { \s i -> return (Unit, lineNumber s) }
<0> spell { \s i -> return (Spell, lineNumber s) }





<0> for { \s i -> return (For, lineNumber s) }
<0> each { \s i -> return (Each, lineNumber s) }


<0> self { \s i -> return (Self, lineNumber s) }
<0> soulPoints  { \s i -> return (SoulPoints, lineNumber s) }
<0> in { \s i -> return (In, lineNumber s) }
<0> attack       { \s i -> return (Attack, lineNumber s) }
<0> defense  { \s i -> return (Defense, lineNumber s) }
<0> hp  { \s i -> return (Hp, lineNumber s) }
<0> max  { \s i -> return (Max, lineNumber s) }
<0> range  { \s i -> return (Range, lineNumber s) }
<0> speed  { \s i -> return (Speed, lineNumber s) }
<0> level  { \s i -> return (Level, lineNumber s) }
<0> knowledge  { \s i -> return (Knowledge, lineNumber s) }
<0> earth  { \s i -> return (Earth, lineNumber s) }
<0> fire  { \s i -> return (Fire, lineNumber s) }
<0> water { \s i -> return (Water, lineNumber s) }
<0> air { \s i -> return (Air, lineNumber s) }
<0> spirit { \s i -> return (Spirit, lineNumber s) }
<0> void { \s i -> return (Void, lineNumber s) }
<0> thoughts { \s i -> return (Thoughts, lineNumber s) }
<0> thought { \s i -> return (Thought, lineNumber s) }
<0> friendly { \s i -> return (Friendly, lineNumber s) }
<0> enemy { \s i -> return (Enemy, lineNumber s) }
<0> field { \s i -> return (Field, lineNumber s) }
<0> hand { \s i -> return (Hand, lineNumber s) }
<0> graveyard { \s i -> return (Graveyard, lineNumber s) }
<0> banished { \s i -> return (Banished, lineNumber s) }
<0> send { \s i -> return (Send, lineNumber s) }
<0> from { \s i -> return (From, lineNumber s) }
<0> to { \s i -> return (To, lineNumber s) }
<0> revive { \s i -> return (Revive, lineNumber s) }
<0> restore { \s i -> return (Restore, lineNumber s) }
<0> base { \s i -> return (Base, lineNumber s) }
<0> permanent  { \s i -> return (Permanent, lineNumber s) }
<0> temporary  { \s i -> return (Temporary, lineNumber s) }
<0> engagement  { \s i -> return (Engagement, lineNumber s) }
<0>  if          { \s i -> return (If, lineNumber s) }
<0> where { \s i -> return (Where, lineNumber s) }
<0> then { \s i -> return (Then, lineNumber s) }
<0> next { \s i -> return (Next, lineNumber s) }
<0> select { \s i -> return (Select, lineNumber s) }
<0> unable { \s i -> return (Unable, lineNumber s) }



<0> "+=" { \s i -> return (Increment, lineNumber s) }
<0> "-=" { \s i -> return (Decrement, lineNumber s) }
<0> "*=" { \s i -> return (Stretch, lineNumber s) }
<0> "/=" { \s i -> return (Crush, lineNumber s) }
<0> "%=" { \s i -> return (Contort, lineNumber s) }


<0>  "," { \s i -> return (Comma, lineNumber s) }
<0>  ":"         { \s i -> return (Colon, lineNumber s) }
<0>  "("         { \s i -> return (Lparen, lineNumber s) }
<0>  ")"         { \s i -> return (Rparen, lineNumber s) }
<0>  "{"         { \s i -> return (Lbracket, lineNumber s) }
<0>  "}"         { \s i -> return (Rbracket, lineNumber s) }


<0>  ";"         { \s i -> return (Semicolon, lineNumber s) }
<0>  "."         { \s i -> return (Dot, lineNumber s) }
<0>  ":="         { \s i -> return (Assignment, lineNumber s) }

<0> else         { \s i -> return (Else, lineNumber s) }



<0>  "+"         { \s i -> return (Sum, lineNumber s) }
<0>  "-"         { \s i -> return (Difference, lineNumber s) }
<0>  "*"         { \s i -> return (Product, lineNumber s) }
<0>  "/"         { \s i -> return (Quotient, lineNumber s) }
<0> "%"          {\s i -> return (Mod, lineNumber s) }



<0> "=" {\s i -> return (Equality, lineNumber s) } 
<0> "<=" {\s i -> return (LEQ, lineNumber s)}
<0> "<" {\s i -> return (Lt, lineNumber s)}
<0> ">=" {\s i -> return (GEQ, lineNumber s)}
<0> ">" {\s i -> return (Gt, lineNumber s)}
<0> "and" {\s i -> return (And, lineNumber s)}
<0> "or" {\s i -> return (Or, lineNumber s)}
<0> "not" {\s i -> return (Not, lineNumber s)}

eof  {\s i -> return (EOFToken, lineNumber s)}



<0> "//"(.*)           {begin 0}

<0> "/*"               {begin comment2}
<comment2> "*/"          {begin 0}

<comment2> [.\n]       ;






 <0> \"\"\" {begin string2}


<string2> ((([.\n]?[.\n]?[^\"])* \"\"\") | \"\"\")  { \s i -> ( (alexSetStartCode 0) >> return ((TargetString $ take (i-3) $ project    s), lineNumber s))}



<0>  \"           {begin string1}
<string1> ([. # \" # \\] | \\0 | \\b | \\t | \\n | \\r | \\f | \\\" | \\\\ )* \"  { \s i -> ( (alexSetStartCode 0) >> return ((TargetString $ (take (i-1) $ project s)), lineNumber s))}





<0>       [0-9]+    { \s i -> return ((Number $ take i $ project s), lineNumber s)}



<0> [a-z] { \s i -> return ((Identifier $ take i $ project s), lineNumber s)}


[.\n] { \s i -> (alexSetStartCode 0) >> return ((Error $ take 1 $ project s), lineNumber s)} -- not sure what to do here.


{

lineNumber ((AlexPn _ s _),_,_,_) = s


-- Some code taken from https://www.haskell.org/alex/doc/alex.pdf, possibly other mostly official references.
-- I also used this tutorial https://www.jyotirmoy.net/posts/2015-08-17-alex-happy-startcodes.html




project (_,_,_,x) = x






-- Each action has type :: String -> Token

-- The token type:
data Token =
  Program
  | Else
  | If
  | Identifier String
  | Colon
  | Lparen
  | Rparen
  | Lbracket
  | Rbracket
  | Comma
  | Semicolon
  | Dot
  | Assignment
  | Mod
  | Sum
  | Difference
  | Product
  | Quotient
  | Number String

  | TargetString String
  | Error String

  | Equality
  | LEQ
  | Lt
  | GEQ
  | Gt
  | And
  | Or
  | Not
 
  | Self
  | Attack
  | Defense
  | Hp
  | Max
  | Range
  | Speed
  | Level
  | Knowledge
  | Earth
  | Fire
  | Water
  | Air
  | Spirit
  | Void
  | Thoughts
  | Thought
  | Friendly
  | Enemy
  | Field
  | Hand
  | Graveyard
  | Banished
  | Spawn
  | Union
  | Send
  | From
  | To
  | Revive
  | Restore
  | Base
  | Permanent
  | Temporary
  | Engagement
  | In
  | Where
  | Then
  | Next
  | Select
  | Unable

  | Increment
  | Decrement
  | Stretch
  | Crush
  | Contort


  | For
  | Each
  

  | Unit
  | Spell


  | Cost
  | SoulPoints
  | SoulSkill
  | StartSkill
  | EndSkill
  | CounterSkill
  | DeathSkill
  | AutoSkill
  | ActionSkill
  | SpawnSkill
  | Condition
  | EOFToken
  deriving (Eq,Show)

alexEOF = return (EOFToken,undefined)




helpPrint :: Int -> String -> String -> String
helpPrint n t val = (show n) ++ "  " ++ t ++ " \"" ++ val ++ "\""

myPrint :: (Token, Int) -> Either String String
myPrint (Else, n) = Left $ helpPrint n "ELSE" "else"
myPrint (If, n) = Left $ helpPrint n "IF" "if"
myPrint (Identifier val, n) = Left $ helpPrint n "IDENT" val
myPrint (Colon, n) = Left $ helpPrint n ":" ":"
myPrint (Lparen, n) = Left $ helpPrint n "(" "("
myPrint (Rparen, n) = Left $ helpPrint n ")" ")"
myPrint (Lbracket, n) = Left $ helpPrint n "{" "{"
myPrint (Rbracket, n) = Left $ helpPrint n "}" "}"
myPrint (Comma, n) = Left $ helpPrint n "," ","
myPrint (Semicolon, n) = Left $ helpPrint n ";" ";"
myPrint (Dot, n) = Left $ helpPrint n "." "."
myPrint (Mod, n) = Left $ helpPrint n "%" "%"
myPrint (Assignment, n) = Left $ helpPrint n "ASSIGNMENT" "assignment"
myPrint (Sum, n) = Left $ helpPrint n "+" "+"
myPrint (Difference, n) = Left $ helpPrint n "-" "-"
myPrint (Product, n) = Left $ helpPrint n "*" "*"
myPrint (Quotient, n) = Left $ helpPrint n "/" "/"
myPrint (Number val, n) = Left $ helpPrint n "INT_LIT" val
myPrint (TargetString val, n) = Left $ helpPrint n "STRING_LIT" val {- I should not add extra quotes here, but I do so that this will be easier to grade. -}
myPrint (Error val, n) = Right $ helpPrint n "ERROR" val

myPrint (Equality, n) = Left $ helpPrint n "EQUALS" "=="
myPrint (LEQ, n) = Left $ helpPrint n "<=" "<=" 
myPrint (Lt, n) = Left $ helpPrint n "<" "<"
myPrint (GEQ, n) = Left $ helpPrint n ">=" ">="
myPrint (Gt, n) = Left $ helpPrint n ">" ">"
myPrint (And, n) = Left $ helpPrint n "AND" "and"
myPrint (Or, n) = Left $ helpPrint n "OR" "or"
myPrint (Not, n) = Left $ helpPrint n "NOT" "not"



myPrint (EOFToken, _) = Left "" {-do I have to worry about an extra newline?-}


fooPrint :: Either String String -> IO ()
fooPrint (Left s) = putStrLn s
fooPrint (Right s) = hPutStrLn stderr s




-- Philip Overgod code:

gather =  alexMonadScan >>= \x -> 
  case x of 
   (EOFToken,n) -> return []
   _ -> gather >>= (return . (x :))

-- end Overgod code

helper s = case runAlex s gather of
                Left _ -> mapM putStrLn [] {- print $ map myPrint (runAlex s (gather)) -}
                Right x -> mapM print x {-fooPrint (map myPrint x)-}
{-
main = do
    s <- getContents
    helper s

-}

} 


