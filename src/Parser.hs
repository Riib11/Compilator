module Parser where

import Utility
import Specification
import Tag

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

parse :: Specification -> [String] -> ParseTree
parse spec tokens = let
  list = parse_to_list spec tokens
  tree = parse_to_tree spec list
  in tree

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

parse_to_list :: Specification -> [String] -> ParseList
parse_to_list spec tokens =
  case tokens of
    []         -> []
    ts@(t:ts') ->
      case take_ptag spec ts of
        Nothing         -> PLIString t  : parse_to_list spec ts'
        Just (pt, ts'') -> PLIPTag   pt : parse_to_list spec ts''

take_ptag :: Specification -> [String] -> Maybe (ParseTag, [String])
take_ptag spec ts@(t:ts') = let
  
  take_name :: [String] -> (String, Bool, [String])
  take_name ts = case ts of
    []    -> error "unexpected end of input; expected tag name"
    t:ts' -> let
      (full_name, rest) = extract_word (word_splits spec) t
      (name, is_end) = case extract (tag_end spec) full_name of
        Nothing        -> (full_name, False)
        Just base_name -> (base_name, True)
      in (name, is_end, rest:ts')
  
  take_args :: [String] -> ([String], [String])
  take_args ts = case ts of
    []    -> error "unexpected end of input; expected tag args or tag close"
    t:ts' -> if t == tag_close spec
      then ([], ts)                                         -- close of tag
      else if t == tag_argsep spec
        then let (as, ts'') = take_args ts' in (as, ts'')   -- close of arg
        else let (as, ts'') = take_args ts' in (t:as, ts'') -- new arg

  take_tag_close :: [String] -> [String]
  take_tag_close ts = case ts of
    []    -> error "unexpected end of input; expected tag close"
    t:ts' -> if t == tag_close spec
      then ts'
      else error $ "expected tag close: " ++ show ts
  
  in if t /= tag_open spec
    -- not a tag
    then Nothing
    -- is a tag
    else let
      -- first `t` was tag open
      (name, is_end, ts1) = take_name ts'
      (args,         ts2) = take_args ts1
      ts3                 = take_tag_close ts2
      in Just $ (PTag name is_end args, ts3)

type ParseList = [ParseListItem]
data ParseListItem = PLIPTag ParseTag | PLIString String deriving (Show)

data ParseTag = PTag
  { p_name   :: String
  , p_is_end :: Bool
  , p_args   :: [String] }
  deriving (Show)


------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

parse_to_tree :: Specification -> ParseList -> ParseTree
parse_to_tree spec list = let

  rec :: ParseList -> ParseTree -> (ParseTree, ParseList)
  rec list parent = case list of
    []     -> (parent, [])
    (x:xs) -> case x of
      PLIPTag pt ->
        let child = PTLeafT (parsetag_to_tag pt)
        in if pt `is_end_ptag_of` (pt_tag parent)
          -- is end tag of parent
          then (parent, [])
          -- is a new tag
          else if (is_container.env.tag_class.pt_tag) child
            -- is a container
            then let
              (child', xs') = rec xs child'
              parent' = add_child child' parent
              in rec xs' parent'
            -- is not a container
            else let
              parent' = add_child child parent
              in rec xs parent'
      PLIString s -> let
        child = PTLeafS s
        parent' = add_child child parent
        in rec xs parent'

  in fst $ rec list (PTRoot [])

is_end_ptag_of :: ParseTag -> Tag -> Bool
is_end_ptag_of ptag tag = ((p_name ptag) == (name.tag_class $ tag)) && (p_is_end ptag)

parsetag_to_tag :: ParseTag -> Tag
parsetag_to_tag pt = error "unimplemented"


data ParseTree
  = PTRoot   { children :: [ParseTree] }
  | PTBranch { pt_tag   :: Tag, children :: [ParseTree] }
  | PTLeafT  { pt_tag   :: Tag }
  | PTLeafS  { pt_str   :: String }
  deriving (Show)

add_child :: ParseTree -> ParseTree -> ParseTree
add_child child parent = case parent of
  PTRoot     cs -> PTRoot     $ cs ++ [child]
  PTBranch t cs -> PTBranch t $ cs ++ [child]
  PTLeafT  t    -> PTBranch t $ [child]
  PTLeafS  s    -> error $ "tried to add child to parent PTLeafS: " ++ show (child, parent)

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
