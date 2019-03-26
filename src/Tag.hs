module Tag where

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

data TagClass = TagClass
  { name  :: String
  , arity :: Arity
  , env   :: Environment
  }
  deriving (Show)

data Arity
  = ArityN Nat
  | ArityS
  deriving (Show)

data Environment = Environment
  { is_container :: Bool
  , is_verbatim  :: Bool
  }
  deriving (Show)

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

data Tag = Tag
  { tag_class :: TagClass
  , arguments :: [String]
  }
  deriving (Show)


------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
