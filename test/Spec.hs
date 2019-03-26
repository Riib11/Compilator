import Prelude hiding (lex)
import Specification
import Tag
import Lexer
import Parser
import Compiler

specification = Specification
  { tag_open    = "<"
  , tag_close   = ">"
  , tag_argsep  = "|"
  , tag_end     = "/"
  , tag_classes = []
  }

source = "<t>This is the contents of a tag</t>"

main = do
  putStrLn ""
  putStrLn $ take 50 $ repeat '-'
  -- print $ lex specification source
  print $
    ( take_ptag specification
    . lex specification
    ) source
