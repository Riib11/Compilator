import Specification
import Tag
import Lexer
import Parser
import Compiler

spec = Specification _

source = _

main = do
  putStrLn ""
  putStrLn $ take 50 $ repeat '-'
  -- parse parser_params source_text >>= print
  print $ parse parser_params source_text
