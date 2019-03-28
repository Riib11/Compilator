module Specification where

import Utility
import Tag

data Specification = Specification
  
  { tag_open              :: String
  , tag_close             :: String
  , tag_argsep            :: String
  , tag_end               :: String
  
  , tag_classes           :: [TagClass]
  
  , tag_sections          :: [String]
  , default_section       :: String
  
  , compile_section_begin :: String -> String
  , compile_section_end   :: String -> String

  , compile_tag_open      :: Tag -> String
  , compile_tag_arg       :: Tag -> Int -> String -> String
  , compile_tag_close     :: Tag -> String
    
  , compile_tag_begin     :: Tag -> String
  , compile_tag_end       :: Tag -> String }


reserveds :: Specification -> [String]
reserveds spec = map (\f -> f spec) [tag_open, tag_close, tag_argsep]

word_splits :: Specification -> [String]
word_splits spec = [" "] ++ reserveds spec
