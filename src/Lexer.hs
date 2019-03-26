module Lexer where

import Utility
import Specification

-- breaks source into tokens according to specification
lex :: Specification -> String -> [String]
lex spec source = let
  rec :: String -> String -> [String]
  rec w s =
    case source of
      ""   -> wrap w
      c:s' ->
        case extract (tag_open spec) s of
          Nothing -> rec (w ++ [c]) s'
          Just s' -> wrap w ++ [tag_open spec] ++ rec "" s'
  in rec "" source
