{

module Lexer where

import System.IO
}

%wrapper "monad"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
<0>  $white+				;
<0> soulSkill { \s i -> return (Token SoulSkill $ SurfaceData (lineNumber s) (columnNumber s) "soulSkill") }
<0> start { \s i -> return (Token StartSkill $ SurfaceData (lineNumber s) (columnNumber s) "start")}
<0> end { \s i -> return (Token EndSkill $ SurfaceData (lineNumber s) (columnNumber s) "end")}
<0> counter { \s i -> return (Token CounterSkill $ SurfaceData (lineNumber s) (columnNumber s) "counter")}
<0> spawn { \s i -> return (Token SpawnSkill $ SurfaceData (lineNumber s) (columnNumber s) "spawn")}
<0> dead { \s i -> return (Token Dead $ SurfaceData (lineNumber s) (columnNumber s) "dead")}
<0> death { \s i -> return (Token DeathSkill $ SurfaceData (lineNumber s) (columnNumber s) "death")}
<0> auto { \s i -> return (Token AutoSkill $ SurfaceData (lineNumber s) (columnNumber s) "auto")}
<0> action { \s i -> return (Token ActionSkill $ SurfaceData (lineNumber s) (columnNumber s) "action")}


<0> cardinality { \s i -> return (Token Cardinality $ SurfaceData (lineNumber s) (columnNumber s) "cardinality")}
<0> damage { \s i -> return (Token Damage $ SurfaceData (lineNumber s) (columnNumber s) "damage")}

<0> position { \s i -> return (Token LexerPosition $ SurfaceData (lineNumber s) (columnNumber s) "position")}
<0> on { \s i -> return (Token LexerOn $ SurfaceData (lineNumber s) (columnNumber s) "on")}
<0> the { \s i -> return (Token LexerThe $ SurfaceData (lineNumber s) (columnNumber s) "the")}
<0> same { \s i -> return (Token LexerSame $ SurfaceData (lineNumber s) (columnNumber s) "same")}
<0> square { \s i -> return (Token LexerSquare $ SurfaceData (lineNumber s) (columnNumber s) "square")}
<0> as { \s i -> return (Token LexerAs $ SurfaceData (lineNumber s) (columnNumber s) "as")}
<0> left { \s i -> return (Token LexerLeft $ SurfaceData (lineNumber s) (columnNumber s) "left")}
<0> of { \s i -> return (Token LexerOf $ SurfaceData (lineNumber s) (columnNumber s) "of")}
<0> right { \s i -> return (Token LexerRight $ SurfaceData (lineNumber s) (columnNumber s) "right")}
<0> front { \s i -> return (Token LexerFront $ SurfaceData (lineNumber s) (columnNumber s) "front")}
<0> behind { \s i -> return (Token LexerBehind $ SurfaceData (lineNumber s) (columnNumber s) "behind")}
<0> this { \s i -> return (Token LexerThis $ SurfaceData (lineNumber s) (columnNumber s) "this")}



<0> cost { \s i -> return (Token Cost $ SurfaceData (lineNumber s) (columnNumber s) "cost")}
<0> condition { \s i -> return (Token Condition $ SurfaceData (lineNumber s) (columnNumber s) "condition")}

<0> union { \s i -> return (Token Union $ SurfaceData (lineNumber s) (columnNumber s) "union")}
<0> unit { \s i -> return (Token Unit $ SurfaceData (lineNumber s) (columnNumber s) "unit")}
<0> spell { \s i -> return (Token Spell $ SurfaceData (lineNumber s) (columnNumber s) "spell")}
<0> for { \s i -> return (Token For $ SurfaceData (lineNumber s) (columnNumber s) "for")}
<0> each { \s i -> return (Token Each $ SurfaceData (lineNumber s) (columnNumber s) "each")}
<0> self { \s i -> return (Token Self $ SurfaceData (lineNumber s) (columnNumber s) "self")}
<0> soulPoints  { \s i -> return (Token SoulPoints $ SurfaceData (lineNumber s) (columnNumber s) "soulPoints")}
<0> in { \s i -> return (Token In $ SurfaceData (lineNumber s) (columnNumber s) "in")}
<0> attack       { \s i -> return (Token Attack $ SurfaceData (lineNumber s) (columnNumber s) "attack")}
<0> defense  { \s i -> return (Token Defense $ SurfaceData (lineNumber s) (columnNumber s) "defense")}
<0> hp  { \s i -> return (Token Hp $ SurfaceData (lineNumber s) (columnNumber s) "hp")}
<0> max  { \s i -> return (Token Max $ SurfaceData (lineNumber s) (columnNumber s) "max")}
<0> range  { \s i -> return (Token Range $ SurfaceData (lineNumber s) (columnNumber s) "range") }
<0> speed  { \s i -> return (Token Speed $ SurfaceData (lineNumber s) (columnNumber s) "speed")}
<0> level  { \s i -> return (Token Level $ SurfaceData (lineNumber s) (columnNumber s) "level")}
<0> knowledge  { \s i -> return (Token Knowledge $ SurfaceData (lineNumber s) (columnNumber s) "knowledge") }
<0> thoughts { \s i -> return (Token Thoughts $ SurfaceData (lineNumber s) (columnNumber s) "thoughts") }
<0> thought { \s i -> return (Token Thought $ SurfaceData (lineNumber s) (columnNumber s) "thought")}
<0> friendly { \s i -> return (Token Friendly $ SurfaceData (lineNumber s) (columnNumber s) "friendly") }
<0> enemy { \s i -> return (Token Enemy $ SurfaceData (lineNumber s) (columnNumber s) "enemy")}
<0> field { \s i -> return (Token Field $ SurfaceData (lineNumber s) (columnNumber s) "field")}
<0> hand { \s i -> return (Token Hand $ SurfaceData (lineNumber s) (columnNumber s) "hand")}
<0> graveyard { \s i -> return (Token Graveyard $ SurfaceData (lineNumber s) (columnNumber s) "graveyard")}
<0> banished { \s i -> return (Token Banished $ SurfaceData (lineNumber s) (columnNumber s) "banished") }
<0> send { \s i -> return (Token Send $ SurfaceData (lineNumber s) (columnNumber s) "send")}
<0> from { \s i -> return (Token From $ SurfaceData (lineNumber s) (columnNumber s) "from")}
<0> to { \s i -> return (Token To $ SurfaceData (lineNumber s) (columnNumber s) "to" )}
<0> revive { \s i -> return (Token Revive $ SurfaceData (lineNumber s) (columnNumber s) "revive") }
<0> restore { \s i -> return (Token Restore $ SurfaceData (lineNumber s) (columnNumber s) "restore") }
<0> base { \s i -> return (Token Base $ SurfaceData (lineNumber s) (columnNumber s) "base")}
<0> permanent  { \s i -> return (Token Permanent $ SurfaceData (lineNumber s) (columnNumber s) "permanent")}
<0> temporary  { \s i -> return (Token Temporary $ SurfaceData (lineNumber s) (columnNumber s) "temporary")}
<0> engagement  { \s i -> return (Token Engagement $ SurfaceData (lineNumber s) (columnNumber s) "engagement") }
<0>  if          { \s i -> return (Token If $ SurfaceData (lineNumber s) (columnNumber s) "if")}
<0> where { \s i -> return (Token Where $ SurfaceData (lineNumber s) (columnNumber s) "where")}
<0> then { \s i -> return (Token Then $ SurfaceData (lineNumber s) (columnNumber s) "then") }
<0> next { \s i -> return (Token Next $ SurfaceData (lineNumber s) (columnNumber s) "next")}
<0> select { \s i -> return (Token Select $ SurfaceData (lineNumber s) (columnNumber s) "select")}
<0> unable { \s i -> return (Token Unable $ SurfaceData (lineNumber s) (columnNumber s) "unable")}
<0> "+=" { \s i -> return (Token Increment $ SurfaceData (lineNumber s) (columnNumber s) "+=")}
<0> "-=" { \s i -> return (Token Decrement $ SurfaceData (lineNumber s) (columnNumber s) "-=")}
<0> "*=" { \s i -> return (Token Stretch $ SurfaceData (lineNumber s) (columnNumber s) "*=") }
<0> "/=" { \s i -> return (Token Crush $ SurfaceData (lineNumber s) (columnNumber s) "/=")}
<0> "%=" { \s i -> return (Token Contort $ SurfaceData (lineNumber s) (columnNumber s) "%=" )}
<0>  "," { \s i -> return (Token Comma $ SurfaceData (lineNumber s) (columnNumber s) ",") }
<0>  ":"         { \s i -> return (Token Colon $ SurfaceData (lineNumber s) (columnNumber s) ":")}
<0>  "("         { \s i -> return (Token Lparen $ SurfaceData (lineNumber s) (columnNumber s) "(")}
<0>  ")"         { \s i -> return (Token Rparen $ SurfaceData (lineNumber s) (columnNumber s) ")")}
<0>  "{"         { \s i -> return (Token Lbracket $ SurfaceData (lineNumber s) (columnNumber s) "{") }
<0>  "}"         { \s i -> return (Token Rbracket $ SurfaceData (lineNumber s) (columnNumber s) "}") }
<0>  ";"         { \s i -> return (Token Semicolon $ SurfaceData (lineNumber s) (columnNumber s) ";") }
<0>  "."         { \s i -> return (Token Dot $ SurfaceData (lineNumber s) (columnNumber s) ".") }
<0>  ":="         { \s i -> return (Token Assignment $ SurfaceData (lineNumber s) (columnNumber s) ":=") }
<0> else         { \s i -> return (Token Else $ SurfaceData (lineNumber s) (columnNumber s) "else") }
<0>  "+"         { \s i -> return (Token Sum $ SurfaceData (lineNumber s) (columnNumber s) "+" )}
<0>  "-"         { \s i -> return (Token Difference $ SurfaceData (lineNumber s) (columnNumber s) "-") }
<0>  "*"         { \s i -> return (Token Product $ SurfaceData (lineNumber s) (columnNumber s) "*") }
<0>  "/"         { \s i -> return (Token Quotient $ SurfaceData (lineNumber s) (columnNumber s) "/") }
<0> "%"          {\s i -> return (Token Mod $ SurfaceData (lineNumber s) (columnNumber s) "%") }
<0> "=" {\s i -> return (Token Equality $ SurfaceData (lineNumber s) (columnNumber s) "=") } 
<0> "<=" {\s i -> return (Token LEQ $ SurfaceData (lineNumber s) (columnNumber s) "<=")}
<0> "<" {\s i -> return (Token Lt $ SurfaceData (lineNumber s) (columnNumber s) "<")}
<0> ">=" {\s i -> return (Token GEQ $ SurfaceData (lineNumber s) (columnNumber s) ">=")}
<0> ">" {\s i -> return (Token Gt $ SurfaceData (lineNumber s) (columnNumber s) ">")}
<0> "and" {\s i -> return (Token And $ SurfaceData (lineNumber s) (columnNumber s) "and")}
<0> "or" {\s i -> return (Token Or $ SurfaceData (lineNumber s) (columnNumber s) "or")}
<0> "not" {\s i -> return (Token Not $ SurfaceData (lineNumber s) (columnNumber s) "not")}
eof  {\s i -> return (Token EOFToken $ SurfaceData (lineNumber s) (columnNumber s) "EOF")}
<0> "//"(.*)           {begin 0}
<0> "/*"               {begin comment2}
<comment2> "*/"          {begin 0}
<comment2> [.\n]       ;
<0>  \"           {begin string1}
<string1> ([. # \" # \\] | \\0 | \\b | \\t | \\n | \\r | \\f | \\\" | \\\\ )* \"  { \s i -> ( (alexSetStartCode 0) >> return ( Token (TargetString (take (i-1) $ project s)) $ SurfaceData (lineNumber s) (columnNumber s) (take (i-1) $ project s)))}
<0>       [0-9]+    { \s i -> return (Token (Number $ take i $ project s) $ SurfaceData (lineNumber s) (columnNumber s) (take i $ project s))}
<0> [a-z] { \s i -> return (Token (Identifier $ take i $ project s) $ SurfaceData (lineNumber s) (columnNumber s) (take i $ project s))}
[.\n] { \s i -> (alexSetStartCode 0) >> return (Token (Error $ take 1 $ project s) $ SurfaceData (lineNumber s) (columnNumber s) "ERROR")} -- not sure what to do here.
<0> [a-z]+ { \s i -> return (Token (Word $ take i $ project s) $ SurfaceData (lineNumber s) (columnNumber s) (take i $ project s))}


{
lineNumber ((AlexPn _ s _),_,_,_) = s
columnNumber ((AlexPn _ _ s),_,_,_) = s
project (_,_,_,x) = x
data Token = Token TokenInner SurfaceData
           deriving Show
data SurfaceData = SurfaceData Int Int String
                 deriving Show




data TokenInner =
  Program
  | LexerPosition
  | LexerOn
  | LexerThe
  | LexerSame
  | LexerSquare
  | LexerAs
  | LexerLeft
  | LexerOf
  | LexerRight
  | LexerFront
  | LexerBehind
  | LexerThis
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

  | Cardinality
  | Damage

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
  | Dead
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
alexEOF = return (Token EOFToken $ SurfaceData (-1) (-1) "EOF")
getLineNumber :: Alex (Int,Int)
getLineNumber = Alex $ \s -> Right (s, myGetLineNumber $ alex_pos s)
myGetLineNumber (AlexPn _ s column) = (s,column)
gather =  alexMonadScan >>= \x -> 
  case x of 
   (Token EOFToken (SurfaceData n m o)) -> return []
   _ -> gather >>= (return . (x :))
helper s = case runAlex s gather of
                Left _ -> mapM putStrLn []
                Right x -> mapM print x








} 


