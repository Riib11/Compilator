module Specification where

import Tag

data Specification = Specification
  { tag_open    :: String
  , tag_close   :: String
  , tag_argsep  :: String
  , tag_classes :: [TagClass]
  }
  deriving (Show)
