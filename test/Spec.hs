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
  , tag_classes = [ TagClass "t" (ArityI 0) (Environment True False) ]
  }

source = "<t>This is the contents of a tag</t><t>This is the contents of a tag</t>"

section = putStrLn $ "\n" ++ (take 50 $ repeat '=')

main = do
  section
  let lexed = lex specification source
  print $ lexed

  section  
  -- let parsed = parse specification lexed
  parsed <- parseIO specification lexed
  putStrLn ""
  
