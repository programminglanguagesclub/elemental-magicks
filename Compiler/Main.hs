module Main where
import qualified Lexer
import qualified Parser
import qualified Typechecker
import qualified Codegen



main = do
 x <- getContents
 case Lexer.runAlex x Parser.calc of
  Right y -> error $ show $ (Parser.prettyPrint $ map Parser.extractSurface (Parser.getTokens x)) {-show y-}
  Left y -> error $ show {-x-} y
            

