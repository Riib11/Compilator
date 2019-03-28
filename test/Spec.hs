import Prelude hiding (lex)
import Specification
import Tag
import Lexer
import Parser
import Compiler

spec = Specification
  { tag_open     = "<"
  , tag_close    = ">"
  , tag_argsep   = "|"
  , tag_end      = "/"
  
  , tag_classes =
      [ TagClass "t" (ArityI 0) (Environment True False) "body"
      , TagClass "f" (ArityI 1) (Environment False False) "head"
      ]
  
  , tag_sections = ["head", "body", "foot"]
  , default_section = "body"

  , compile_section_begin = \sec -> "\n" ++ "(Begin " ++ sec ++ ")" ++ "\n"
  , compile_section_end   = \sec -> "\n" ++ "(End "   ++ sec ++ ")" ++ "\n"
  
  , compile_tag_open      = \tag -> "(open " ++ (tc_name.tag_class $ tag) ++ ")"
  , compile_tag_arg       = \tag i arg -> " [" ++ show i ++ "]=(" ++ arg ++ ") "
  , compile_tag_close     = \tag -> "(close " ++ (tc_name.tag_class $ tag) ++ ")"
  
  , compile_tag_begin     = \tag -> "(begin " ++ (tc_name.tag_class $ tag) ++ ")"
  , compile_tag_end       = \tag -> "(end " ++ (tc_name.tag_class $ tag) ++ ")"
  }

source = "<f hello world!><t>This is the contents of a tag</t><t>This is the contents of a tag</t>"

main :: IO ()
main = mainIO

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

mainPure :: IO ()
mainPure = putStrLn $ compile spec . parse spec . lex spec $ source

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

mainIO :: IO ()
mainIO = do
  section
  let lexed = lex spec source
  print $ lexed

  section
  -- let parsed = parse spec lexed
  parsed <- parseIO spec lexed
  putStrLn ""
  
  section
  -- let 
  compiled <- compileIO spec parsed
  putStrLn ""

section = putStrLn $ "\n" ++ (take 50 $ repeat '=')
