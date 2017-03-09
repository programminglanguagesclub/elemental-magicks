{

module Lexer where

import System.IO
}

%wrapper "monad"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
<0>  $white+				;
<0> soulSkill { \s i -> return (Token SoulSkill (lineNumber s) (columnNumber s) "soulSkill") }
<0> start { \s i -> return (Token StartSkill  (lineNumber s) (columnNumber s) "start")}
<0> end { \s i -> return (Token EndSkill (lineNumber s) (columnNumber s) "end")}
<0> counter { \s i -> return (Token CounterSkill (lineNumber s) (columnNumber s) "counter")}
<0> spawn { \s i -> return (Token SpawnSkill (lineNumber s) (columnNumber s) "spawn")}
<0> death { \s i -> return (Token DeathSkill (lineNumber s) (columnNumber s) "death")}
<0> auto { \s i -> return (Token AutoSkill (lineNumber s) (columnNumber s) "auto")}
<0> action { \s i -> return (Token ActionSkill (lineNumber s) (columnNumber s) "action")}
<0> union { \s i -> return (Token Union (lineNumber s) (columnNumber s) "union")}
<0> unit { \s i -> return (Token Unit (lineNumber s) (columnNumber s) "unit")}
<0> spell { \s i -> return (Token Spell (lineNumber s) (columnNumber s) "spell")}
<0> for { \s i -> return (Token For (lineNumber s) (columnNumber s) "for")}
<0> each { \s i -> return (Token Each (lineNumber s) (columnNumber s) "each")}
<0> self { \s i -> return (Token Self (lineNumber s) (columnNumber s) "self")}
<0> soulPoints  { \s i -> return (Token SoulPoints (lineNumber s) (columnNumber s) "soulPoints")}
<0> in { \s i -> return (Token In (lineNumber s) (columnNumber s) "in")}
<0> attack       { \s i -> return (Token Attack (lineNumber s) (columnNumber s) "attack")}
<0> defense  { \s i -> return (Token Defense (lineNumber s) (columnNumber s) "defense")}
<0> hp  { \s i -> return (Token Hp (lineNumber s) (columnNumber s) "hp")}
<0> max  { \s i -> return (Token Max (lineNumber s) (columnNumber s) "max")}
<0> range  { \s i -> return (Token Range (lineNumber s) (columnNumber s) "range") }
<0> speed  { \s i -> return (Token Speed (lineNumber s) (columnNumber s) "speed")}
<0> level  { \s i -> return (Token Level (lineNumber s) (columnNumber s) "level")}
<0> knowledge  { \s i -> return (Token Knowledge (lineNumber s) (columnNumber s) "knowledge") }
<0> thoughts { \s i -> return (Token Thoughts (lineNumber s) (columnNumber s) "thoughts") }
<0> thought { \s i -> return (Token Thought (lineNumber s) (columnNumber s) "thought")}
<0> friendly { \s i -> return (Token Friendly (lineNumber s) (columnNumber s) "friendly") }
<0> enemy { \s i -> return (Token Enemy (lineNumber s) (columnNumber s) "enemy")}
<0> field { \s i -> return (Token Field (lineNumber s) (columnNumber s) "field")}
<0> hand { \s i -> return (Token Hand (lineNumber s) (columnNumber s) "hand")}
<0> graveyard { \s i -> return (Token Graveyard (lineNumber s) (columnNumber s) "graveyard")}
<0> banished { \s i -> return (Token Banished (lineNumber s) (columnNumber s) "banished") }
<0> send { \s i -> return (Token Send (lineNumber s) (columnNumber s) "send")}
<0> from { \s i -> return (Token From (lineNumber s) (columnNumber s) "from")}
<0> to { \s i -> return (Token To (lineNumber s) (columnNumber s) "to" )}
<0> revive { \s i -> return (Token Revive (lineNumber s) (columnNumber s) "revive") }
<0> restore { \s i -> return (Token Restore (lineNumber s) (columnNumber s) "restore") }
<0> base { \s i -> return (Token Base (lineNumber s) (columnNumber s) "base")}
<0> permanent  { \s i -> return (Token Permanent (lineNumber s) (columnNumber s) "permanent")}
<0> temporary  { \s i -> return (Token Temporary (lineNumber s) (columnNumber s) "temporary")}
<0> engagement  { \s i -> return (Token Engagement (lineNumber s) (columnNumber s) "engagement") }
<0>  if          { \s i -> return (Token If (lineNumber s) (columnNumber s) "if")}
<0> where { \s i -> return (Token Where (lineNumber s) (columnNumber s) "where")}
<0> then { \s i -> return (Token Then (lineNumber s) (columnNumber s) "then") }
<0> next { \s i -> return (Token Next (lineNumber s) (columnNumber s) "next")}
<0> select { \s i -> return (Token Select (lineNumber s) (columnNumber s) "select")}
<0> unable { \s i -> return (Token Unable (lineNumber s) (columnNumber s) "unable")}
<0> "+=" { \s i -> return (Token Increment (lineNumber s) (columnNumber s) "+=")}
<0> "-=" { \s i -> return (Token Decrement (lineNumber s) (columnNumber s) "-=")}
<0> "*=" { \s i -> return (Token Stretch (lineNumber s) (columnNumber s) "*=") }
<0> "/=" { \s i -> return (Token Crush (lineNumber s) (columnNumber s) "/=")}
<0> "%=" { \s i -> return (Token Contort (lineNumber s) (columnNumber s) "%=" )}
<0>  "," { \s i -> return (Token Comma (lineNumber s) (columnNumber s) ",") }
<0>  ":"         { \s i -> return (Token Colon (lineNumber s) (columnNumber s) ":")}
<0>  "("         { \s i -> return (Token Lparen (lineNumber s) (columnNumber s) "(")}
<0>  ")"         { \s i -> return (Token Rparen (lineNumber s) (columnNumber s) ")")}
<0>  "{"         { \s i -> return (Token Lbracket (lineNumber s) (columnNumber s) "{") }
<0>  "}"         { \s i -> return (Token Rbracket (lineNumber s) (columnNumber s) "}") }
<0>  ";"         { \s i -> return (Token Semicolon (lineNumber s) (columnNumber s) ";") }
<0>  "."         { \s i -> return (Token Dot (lineNumber s) (columnNumber s) ".") }
<0>  ":="         { \s i -> return (Token Assignment (lineNumber s) (columnNumber s) ":=") }
<0> else         { \s i -> return (Token Else (lineNumber s) (columnNumber s) "else") }
<0>  "+"         { \s i -> return (Token Sum (lineNumber s) (columnNumber s) "+" )}
<0>  "-"         { \s i -> return (Token Difference (lineNumber s) (columnNumber s) "-") }
<0>  "*"         { \s i -> return (Token Product (lineNumber s) (columnNumber s) "*") }
<0>  "/"         { \s i -> return (Token Quotient (lineNumber s) (columnNumber s) "/") }
<0> "%"          {\s i -> return (Token Mod (lineNumber s) (columnNumber s) "%") }
<0> "=" {\s i -> return (Token Equality (lineNumber s) (columnNumber s) "=") } 
<0> "<=" {\s i -> return (Token LEQ (lineNumber s) (columnNumber s) "<=")}
<0> "<" {\s i -> return (Token Lt (lineNumber s) (columnNumber s) "<")}
<0> ">=" {\s i -> return (Token GEQ (lineNumber s) (columnNumber s) ">=")}
<0> ">" {\s i -> return (Token Gt (lineNumber s) (columnNumber s) ">")}
<0> "and" {\s i -> return (Token And (lineNumber s) (columnNumber s) "and")}
<0> "or" {\s i -> return (Token Or (lineNumber s) (columnNumber s) "or")}
<0> "not" {\s i -> return (Token Not (lineNumber s) (columnNumber s) "not")}
eof  {\s i -> return (Token EOFToken (lineNumber s) (columnNumber s) "EOF")}
<0> "//"(.*)           {begin 0}
<0> "/*"               {begin comment2}
<comment2> "*/"          {begin 0}
<comment2> [.\n]       ;
<0>  \"           {begin string1}
<string1> ([. # \" # \\] | \\0 | \\b | \\t | \\n | \\r | \\f | \\\" | \\\\ )* \"  { \s i -> ( (alexSetStartCode 0) >> return ( Token (TargetString (take (i-1) $ project s)) (lineNumber s) (columnNumber s) (take (i-1) $ project s)))}
<0>       [0-9]+    { \s i -> return (Token (Number $ take i $ project s) (lineNumber s) (columnNumber s) (take i $ project s))}
<0> [a-z] { \s i -> return (Token (Identifier $ take i $ project s) (lineNumber s) (columnNumber s) (take i $ project s))}
[.\n] { \s i -> (alexSetStartCode 0) >> return (Token (Error $ take 1 $ project s) (lineNumber s) (columnNumber s) "ERROR")} -- not sure what to do here.
<0> [a-z]+ { \s i -> return (Token (Word $ take i $ project s) (lineNumber s) (columnNumber s) (take i $ project s))}


{
lineNumber ((AlexPn _ s _),_,_,_) = s
columnNumber ((AlexPn _ _ s),_,_,_) = s
project (_,_,_,x) = x
data Token = Token TokenInner Int Int String
           deriving Show
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
  | Word String
  | EOFToken
  deriving (Eq,Show)
alexEOF = return (Token EOFToken (-1) (-1) "EOF")
fooPrint :: Either String String -> IO ()
fooPrint (Left s) = putStrLn s
fooPrint (Right s) = hPutStrLn stderr s
getLineNumber :: Alex (Int,Int)
getLineNumber = Alex $ \s -> Right (s, myGetLineNumber $ alex_pos s)
myGetLineNumber (AlexPn _ s column) = (s,column)
gather =  alexMonadScan >>= \x -> 
  case x of 
   (Token EOFToken n m o) -> return []
   _ -> gather >>= (return . (x :))
helper s = case runAlex s gather of
                Left _ -> mapM putStrLn []
                Right x -> mapM print x








} 


