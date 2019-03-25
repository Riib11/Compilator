module Main where

import Prelude hiding (lex)
import Lexer

lp = LexerParameters [" ","<",">"]
source_text = "<p>Hello World!</p>"

main = do