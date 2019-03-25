module Parser where

import Tag

type S = String

data ParseParameters = ParseParameters
  { tag_open   :: S
  , tag_close  :: S
  , tag_argsep :: S
  , tags       :: [Tag]
  } deriving (Show)

parse :: ParseParameters -> S -> ParseTree
parse params source = let

  -- if name beginning matches some tag,
  --   then: Just that tag
  --   else: Nothing
  get_tag_from_name :: S -> Maybe Tag
  get_tag_from_name s = let
    f :: Maybe Tag -> Tag -> Maybe Tag
    f mb_tag tag =
      case mb_tag of
        Just t  -> Just t
        Nothing -> if (name tag) `begins` s then Just tag else Nothing
    in foldl f Nothing (tags params)
  
  -- extracts the next string up to the next tag open
  extract_string :: S -> Maybe (S, S)
  extract_string "" = Nothing
  extract_string s = let
    rec :: S -> S -> (S, S)
    rec s@(c:s') working =
      if (tag_open params) `begins` s then (working, s) else rec (working ++ [c]) s'
    (str, s') = rec s ""
    in if str /= ""
      then Just (str, s')
      else Nothing

  extract_verbatim :: Tag -> S -> (S, S)
  extract_verbatim t s@(c:s') =
    if (name t) `begins` s
      then ("", s)
      else fold c (extract_verbatim t s') where fold c (str, s'') = (c:str, s'')

  extract_tag_open :: S -> Maybe S
  extract_tag_open = extract (tag_open params)

  extract_tag_name :: S -> (S, S)
  extract_tag_name = extract_word . extract_whitespace

  extract_tag_args :: S -> ([S], S)
  extract_tag_args s = let

    rec :: S -> S -> ([S], S)
    rec arg s = let
      s' = extract_whitespace s
      prepend_arg []  (as, s) = (as  , s)
      prepend_arg [a] (as, s) = (a:as, s)
      arg_wrapped = if arg == "" then [] else [arg]
      in
        -- at end of tag
        case extract (tag_close params) s' of
          Just s'' -> (arg_wrapped, s')
          Nothing  ->
            -- at end of arg
            case extract (tag_argsep params) s' of
              Just s'' -> prepend_arg arg_wrapped $ rec "" s''
              Nothing  ->
                -- ready for arg
                case s' of
                  []      -> (arg_wrapped, s')
                  (c:s'') -> rec (c:arg) s''
    in rec "" s

  extract_tag_close :: S -> Maybe S
  extract_tag_close = (extract $ tag_close params) . extract_whitespace

  extract_tag :: S -> Maybe (TagParsed, S)
  extract_tag s =
    case extract_tag_open source of
      -- no tag here
      Nothing -> Nothing
      -- a tag starts here
      Just s1 ->
        let (name, s2) = extract_tag_name s1
        in case get_tag_from_name name of
          Nothing  -> error $ "invalid tag name: " ++ name
          Just tag -> let
            (args, s3) = extract_tag_args s2
            mb_s4      = extract_tag_close s3
            in case mb_s4 of
              Nothing -> error $ "expected tag close"
              Just s4 -> Just (TagParsed tag args, s4)

  rec :: ParseTree -> S -> (ParseTree, S)
  rec parent s = let
    rec_with_child :: ParseTree -> S -> (ParseTree, S)
    rec_with_child child s' = rec (add_child child parent) s'
    in case extract_tag s of
      -- extract tag
      (Just (tp, s')) ->
        case env (tag tp) of
          EnvLeaf     -> rec_with_child child s'  where child        = (ParseLeafT tp)
          EnvBranch   -> rec_with_child child s'' where (child, s'') = rec (ParseBranch tp []) s'
          EnvVerbatim -> rec_with_child child s'' where child        = add_child (ParseLeafS str) parent
                                                        (str, s'')   = extract_verbatim (tag tp) s'
      Nothing -> case extract_string s of
        -- extract string
        (Just (str, s')) -> rec_with_child (ParseLeafS str) s'
        -- end of input
        Nothing -> (parent, s)

  in fst $ rec (ParseBranch TagParsedRoot []) source

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

begins :: S -> S -> Bool
begins target source = case (target, source) of
  ([]  , []  ) -> True
  ([]  , _   ) -> True
  (t:ts, s:ss) -> if t == s then begins ts ss else False

-- if `target` begins `source`,
--   then, Just `source` with the beginning `target` removed
--   else, Nothing
extract :: S -> S -> Maybe S
extract target source =
  if target `begins` source
    then Just $ drop (length target) source
    else Nothing

-- `source` => the first word of `source, `source` with the first word removed
extract_word :: S -> (S, S)
extract_word []       = ([], [])
extract_word (' ':cs) = ([], ' ':cs)
extract_word (c  :cs) = (c:word, rest) where (word, rest) = extract_word cs

extract_whitespace :: S -> S
extract_whitespace s = case s of
  (' ' :cs) -> extract_whitespace cs
  ('\n':cs) -> extract_whitespace cs
  _         -> s

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data TagParsed
  = TagParsed
      { tag  :: Tag
      , args :: [String] } 
  | TagParsedRoot
  deriving (Show)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data ParseTree
  = ParseBranch { val_tag :: TagParsed, children :: [ParseTree] }
  | ParseLeafT  { val_tag :: TagParsed }
  | ParseLeafS  { val_str :: S }
  deriving (Show)

add_child :: ParseTree -> ParseTree -> ParseTree
add_child child parent = case parent of
  ParseBranch t cs -> ParseBranch t $ cs ++ [child]
  ParseLeafT  t    -> ParseBranch t $ [child]
  ParseLeafS  s    -> error $ "attempted to add child to " ++ show parent
