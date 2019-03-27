module Specification where

import Utility
import Tag

data Specification = Specification
  { tag_open    :: String
  , tag_close   :: String
  , tag_argsep  :: String
  , tag_end     :: String
  , tag_classes :: [TagClass]
  }
  deriving (Show)

reserveds :: Specification -> [String]
reserveds spec = map (\f -> f spec) [tag_open, tag_close, tag_argsep]

word_splits :: Specification -> [String]
word_splits spec = [" "] ++ reserveds spec
