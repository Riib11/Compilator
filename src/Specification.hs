{-# LANGUAGE TemplateHaskell #-}

module Specification where

import Control.Lens
import Utility
import Tag

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

data Specification = Specification
  
  { _tag_open              :: String
  , _tag_close             :: String
  , _tag_argsep            :: String
  , _tag_end               :: String
  
  , _tag_classes           :: [TagClass]
  
  , _tag_sections          :: [String]

  , _compile_section_begin :: String -> String
  , _compile_section_end   :: String -> String
  , _compile_tag_open      :: Tag -> String
  , _compile_tag_arg       :: Tag -> Int -> String -> String
  , _compile_tag_close     :: Tag -> String
  , _compile_tag_begin     :: Tag -> String
  , _compile_tag_end       :: Tag -> String

  , _dft_section               :: String
  , _dft_compile_section_begin :: String -> String
  , _dft_compile_section_end   :: String -> String
  , _dft_compile_tag_open      :: Tag -> String
  , _dft_compile_tag_arg       :: Tag -> Int -> String -> String
  , _dft_compile_tag_close     :: Tag -> String
  , _dft_compile_tag_begin     :: Tag -> String
  , _dft_compile_tag_end       :: Tag -> String }

$(makeLenses ''Specification)

reserveds :: Specification -> [String]
reserveds spec = map (\field -> view field spec) [tag_open, tag_close, tag_argsep]

word_splits :: Specification -> [String]
word_splits spec = [" "] ++ reserveds spec

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

add_tag_section :: String -> Specification -> Specification
add_tag_section sec spec = set tag_sections (view tag_sections spec ++ [sec]) spec

add_tag_class :: TagClass -> Specification -> Specification
add_tag_class tc spec = set tag_classes (view tag_classes spec ++ [tc]) spec
