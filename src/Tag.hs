module Tag where

import Natural

type TagName = String

data Environment
  = EnvLeaf
  | EnvBranch
  | EnvVerbatim 
  deriving (Show)

data Arity
  = ArityN Nat
  | ArityS

instance Show Arity where
  show (ArityN n) = show n
  show ArityS = "*"

data Tag = Tag
  { name  :: TagName
  , arity :: Arity
  , env   :: Environment
  }

instance Show Tag where
  show (Tag n a EnvBranch) = "<" ++ n ++ "(" ++ show a ++ ")::EnvBranch>"
  show (Tag n a _) = "<" ++ n ++ "(" ++ show a ++ ")>"
