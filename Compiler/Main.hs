module Main where
import qualified Lexer
import qualified Parser
import qualified Typechecker
import qualified Codegen
import Text.Read
import qualified Data.Map.Strict as HashMap


main = do
 x <- getContents
 case Lexer.runAlex x Parser.calc of
  Right y -> error $ show $ (Parser.prettyPrint $ map Parser.extractSurface (Parser.getTokens x)) {-show y-}
  Left y ->  {- assume no error in lexing... -}
   let tokens = Parser.getTokens x in
   let tokenLocation = Parser.generateTokenLocation tokens in
    case (readMaybe y :: Maybe (Int,Int)) of
     Nothing -> error "Line, column numbers caused unexpected fatal error. Sorry."
     Just (line,column) -> let v = HashMap.lookup (line,column+1) tokenLocation in
      case v of
       Nothing ->
        error ("Reached end of file while parsing (or other error).")
       Just surface ->
        case surface of "ERROR" -> error ("Lex error on line " ++ (show line) ++ ", column " ++ (show column))
                        _ -> error ("Parse error on token " ++ surface ++ " on line " ++ (show line) ++ ", column " ++ (show column))
   {-let token = tokenLocation.lookup y in-}
            


{-I SHOULD ADD SPELLCHECK ON LEX AND PARSE ERRORS....-}

