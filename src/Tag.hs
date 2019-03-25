module Tag where

import Natural

type TagName = String

data Environment
  = EnvLeaf
  | EnvBranch
  | EnvVerbatim 
  deriving (Show)

data Count
  = CountNat Nat
  | CountStar
  deriving (Show)

data Tag = Tag
  { name  :: TagName
  , env   :: Environment
  , arity :: Count
  } deriving (Show)
