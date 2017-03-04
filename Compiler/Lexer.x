{

module Lexer where

import System.IO
}

%wrapper "monad"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters



tokens :-
<0>  $white+				;

<0> soulSkill { \s i -> return (Token SoulSkill $ lineNumber s) }
<0> start { \s i -> return (Token StartSkill $ lineNumber s) }
<0> end { \s i -> return (Token EndSkill $ lineNumber s) }
<0> counter { \s i -> return (Token CounterSkill $ lineNumber s) }
<0> spawn { \s i -> return (Token SpawnSkill $ lineNumber s) }
<0> death { \s i -> return (Token DeathSkill $ lineNumber s) }
<0> auto { \s i -> return (Token AutoSkill $ lineNumber s) }
<0> action { \s i -> return (Token ActionSkill $ lineNumber s) }
<0> union { \s i -> return (Token Union $ lineNumber s) }


<0> unit { \s i -> return (Token Unit $ lineNumber s) }
<0> spell { \s i -> return (Token Spell $ lineNumber s) }





<0> for { \s i -> return (Token For $ lineNumber s) }
<0> each { \s i -> return (Token Each $ lineNumber s) }


<0> self { \s i -> return (Token Self $ lineNumber s) }
<0> soulPoints  { \s i -> return (Token SoulPoints $ lineNumber s) }
<0> in { \s i -> return (Token In $ lineNumber s) }
<0> attack       { \s i -> return (Token Attack $ lineNumber s) }
<0> defense  { \s i -> return (Token Defense $ lineNumber s) }
<0> hp  { \s i -> return (Token Hp $ lineNumber s) }
<0> max  { \s i -> return (Token Max $ lineNumber s) }
<0> range  { \s i -> return (Token Range $ lineNumber s) }
<0> speed  { \s i -> return (Token Speed $ lineNumber s) }
<0> level  { \s i -> return (Token Level $ lineNumber s) }
<0> knowledge  { \s i -> return (Token Knowledge $ lineNumber s) }
<0> earth  { \s i -> return (Token Earth $ lineNumber s) }
<0> fire  { \s i -> return (Token Fire $ lineNumber s) }
<0> water { \s i -> return (Token Water $ lineNumber s) }
<0> air { \s i -> return (Token Air $ lineNumber s) }
<0> spirit { \s i -> return (Token Spirit $ lineNumber s) }
<0> void { \s i -> return (Token Void $ lineNumber s) }
<0> thoughts { \s i -> return (Token Thoughts $ lineNumber s) }
<0> thought { \s i -> return (Token Thought $ lineNumber s) }
<0> friendly { \s i -> return (Token Friendly $ lineNumber s) }
<0> enemy { \s i -> return (Token Enemy $ lineNumber s) }
<0> field { \s i -> return (Token Field $ lineNumber s) }
<0> hand { \s i -> return (Token Hand $ lineNumber s) }
<0> graveyard { \s i -> return (Token Graveyard $ lineNumber s) }
<0> banished { \s i -> return (Token Banished $ lineNumber s) }
<0> send { \s i -> return (Token Send $ lineNumber s) }
<0> from { \s i -> return (Token From $ lineNumber s) }
<0> to { \s i -> return (Token To $ lineNumber s) }
<0> revive { \s i -> return (Token Revive $ lineNumber s) }
<0> restore { \s i -> return (Token Restore $ lineNumber s) }
<0> base { \s i -> return (Token Base $ lineNumber s) }
<0> permanent  { \s i -> return (Token Permanent $ lineNumber s) }
<0> temporary  { \s i -> return (Token Temporary $ lineNumber s) }
<0> engagement  { \s i -> return (Token Engagement $ lineNumber s) }
<0>  if          { \s i -> return (Token If $ lineNumber s) }
<0> where { \s i -> return (Token Where $ lineNumber s) }
<0> then { \s i -> return (Token Then $ lineNumber s) }
<0> next { \s i -> return (Token Next $ lineNumber s) }
<0> select { \s i -> return (Token Select $ lineNumber s) }
<0> unable { \s i -> return (Token Unable $ lineNumber s) }



<0> "+=" { \s i -> return (Token Increment $ lineNumber s) }
<0> "-=" { \s i -> return (Token Decrement $ lineNumber s) }
<0> "*=" { \s i -> return (Token Stretch $ lineNumber s) }
<0> "/=" { \s i -> return (Token Crush $ lineNumber s) }
<0> "%=" { \s i -> return (Token Contort $ lineNumber s) }


<0>  "," { \s i -> return (Token Comma $ lineNumber s) }
<0>  ":"         { \s i -> return (Token Colon $ lineNumber s) }
<0>  "("         { \s i -> return (Token Lparen $ lineNumber s) }
<0>  ")"         { \s i -> return (Token Rparen $ lineNumber s) }
<0>  "{"         { \s i -> return (Token Lbracket $ lineNumber s) }
<0>  "}"         { \s i -> return (Token Rbracket $ lineNumber s) }


<0>  ";"         { \s i -> return (Token Semicolon $ lineNumber s) }
<0>  "."         { \s i -> return (Token Dot $ lineNumber s) }
<0>  ":="         { \s i -> return (Token Assignment $ lineNumber s) }

<0> else         { \s i -> return (Token Else $ lineNumber s) }



<0>  "+"         { \s i -> return (Token Sum $ lineNumber s) }
<0>  "-"         { \s i -> return (Token Difference $ lineNumber s) }
<0>  "*"         { \s i -> return (Token Product $ lineNumber s) }
<0>  "/"         { \s i -> return (Token Quotient $ lineNumber s) }
<0> "%"          {\s i -> return (Token Mod $ lineNumber s) }



<0> "=" {\s i -> return (Token Equality $ lineNumber s) } 
<0> "<=" {\s i -> return (Token LEQ $ lineNumber s)}
<0> "<" {\s i -> return (Token Lt $ lineNumber s)}
<0> ">=" {\s i -> return (Token GEQ $ lineNumber s)}
<0> ">" {\s i -> return (Token Gt $ lineNumber s)}
<0> "and" {\s i -> return (Token And $ lineNumber s)}
<0> "or" {\s i -> return (Token Or $ lineNumber s)}
<0> "not" {\s i -> return (Token Not $ lineNumber s)}

eof  {\s i -> return (Token EOFToken $ lineNumber s)}



<0> "//"(.*)           {begin 0}

<0> "/*"               {begin comment2}
<comment2> "*/"          {begin 0}

<comment2> [.\n]       ;






 <0> \"\"\" {begin string2}


<string2> ((([.\n]?[.\n]?[^\"])* \"\"\") | \"\"\")  { \s i -> ( (alexSetStartCode 0) >> return ( Token (TargetString $ take (i-3) $ project    s) $ lineNumber s))}



<0>  \"           {begin string1}
<string1> ([. # \" # \\] | \\0 | \\b | \\t | \\n | \\r | \\f | \\\" | \\\\ )* \"  { \s i -> ( (alexSetStartCode 0) >> return ( Token (TargetString $ (take (i-1) $ project s)) $ lineNumber s))}





<0>       [0-9]+    { \s i -> return (Token (Number $ take i $ project s) $ lineNumber s)}



<0> [a-z] { \s i -> return (Token (Identifier $ take i $ project s) $ lineNumber s)}


[.\n] { \s i -> (alexSetStartCode 0) >> return (Token (Error $ take 1 $ project s) $ lineNumber s)} -- not sure what to do here.


{

lineNumber ((AlexPn _ s _),_,_,_) = s


-- Some code taken from https://www.haskell.org/alex/doc/alex.pdf, possibly other mostly official references.
-- I also used this tutorial https://www.jyotirmoy.net/posts/2015-08-17-alex-happy-startcodes.html




project (_,_,_,x) = x






-- Each action has type :: String -> Token

data Token = Token TokenInner Int
           deriving Show
-- The token type:
data TokenInner =
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

alexEOF = return (Token EOFToken (-1))



{-
helpPrint :: Int -> String -> String -> String
helpPrint n t val = (show n) ++ "  " ++ t ++ " \"" ++ val ++ "\""

myPrint :: (TokenInner, Int) -> Either String String
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
-}

fooPrint :: Either String String -> IO ()
fooPrint (Left s) = putStrLn s
fooPrint (Right s) = hPutStrLn stderr s



getLineNumber :: Alex Int
getLineNumber = Alex $ \s -> Right (s, myGetLineNumber $ alex_pos s)
myGetLineNumber (AlexPn _ s _) = s




-- Philip Overgod code:

gather =  alexMonadScan >>= \x -> 
  case x of 
   (Token EOFToken n) -> return []
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


