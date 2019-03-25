import Natural
import Tag
import Parser

parser_params = ParseParameters
  { tag_open   = "<"
  , tag_close  = ">"
  , tag_argsep = "|"
  , tags = [ Tag "t" (ArityN Zero) EnvLeaf ]
  }

source_text = "<t>"

main = do
  putStrLn ""
  putStrLn $ take 50 $ repeat '-'
  print $ parse parser_params source_text
