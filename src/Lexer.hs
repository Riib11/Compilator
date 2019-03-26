module Lexer where

import Prelude hiding (lex)
import Utility
import Specification

-- breaks source into tokens according to specification
lex :: Specification -> String -> [String]
lex spec source = let
  rec :: String -> String -> [String]
  rec w s =
    case s of
      ""   -> wrap w
      c:s' ->
        case extract_any (reserveds spec) s of
          Nothing       -> rec (w ++ [c]) s'
          Just (r, s'') -> wrap w ++ [r] ++ rec "" s''
  in rec "" source
