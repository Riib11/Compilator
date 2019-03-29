module Tag where

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

data TagClass = TagClass
  { tc_name  :: String
  , tc_arity :: Arity
  , tc_env   :: Environment
  , tc_sec   :: String }

instance Show TagClass where
  show (TagClass { tc_name = n, tc_arity = a, tc_sec = s }) =
    s ++ "." ++ n ++ "(" ++ show a ++ ")"

data Arity
  = ArityI Int
  | ArityS

instance Show Arity where
  show (ArityI n) = show n
  show (ArityS  ) = "*"

check_arity :: Arity -> Int -> Bool
check_arity arity n = case arity of
  ArityI n' -> n == n'
  ArityS    -> True

data Environment = Environment
  { env_is_container :: Bool
  , env_is_verbatim  :: Bool
  }
  deriving (Show)

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

data Tag = Tag
  { tag_class :: TagClass
  , tag_args  :: [String] }

instance Show Tag where
  show t = show (tag_class t) ++ show (tag_args t)

instance Eq Tag where
  tag == tag' = (tc_name.tag_class $ tag) == (tc_name.tag_class $ tag')
  tag /= tag' = (tc_name.tag_class $ tag) /= (tc_name.tag_class $ tag')

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

