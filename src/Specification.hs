module Specification where

import Utility
import Tag

data Specification = Specification
  { tag_open    :: String
  , tag_close   :: String
  , tag_argsep  :: String
  , tag_classes :: [TagClass]
  }
  deriving (Show)

test_spec = Specification
  { tag_open    = "<"
  , tag_close   = ">"
  , tag_argsep  = "|"
  , tag_classes = []
  }

reserveds :: Specification -> [String]
reserveds spec = map (\f -> f spec) [tag_open, tag_close, tag_argsep]