module Parser where

import Utility
import Specification
import Tag

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

parse :: Specification -> [String] -> ParseTree
parse spec = parse_to_tree spec . parse_to_list spec

parseIO :: Specification -> [String] -> IO ParseTree
parseIO spec tokens = do
  let list = parse_to_list spec tokens
  putStrLn $ "parse-list:\n" ++ show list
  let tree = parse_to_tree spec list
  putStrLn $ "parse-tree:\n" ++ show tree
  return tree

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

type ParseList = [ParseListItem]
data ParseListItem
  = PLI_PTag ParseTag
  | PLI_String String

instance Show ParseListItem where
  show (PLI_PTag   pt) = show pt
  show (PLI_String s ) = show s

data ParseTag = PTag
  { pt_name   :: String
  , pt_is_end :: Bool
  , pt_args   :: [String] }
  deriving (Show)

parse_to_list :: Specification -> [String] -> ParseList
parse_to_list spec tokens =
  case tokens of
    []         -> []
    ts@(t:ts') ->
      case take_ptag spec ts of
        Nothing         -> PLI_String t  : parse_to_list spec ts'
        Just (pt, ts'') -> PLI_PTag   pt : parse_to_list spec ts''

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
      else if (t == tag_argsep spec) || (t == "")
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

pt_base_name :: Specification -> ParseTag -> String
pt_base_name spec (PTag {pt_name = name}) =
  case extract (tag_end spec) name of { Just name' -> name'; Nothing -> name }

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

data ParseTree
  = PT_Root   { children :: [ParseTree] }
  | PT_Branch { pt_tag   :: Tag, children :: [ParseTree] }
  | PT_LeafT  { pt_tag   :: Tag }
  | PT_LeafS  { pt_str   :: String }

instance Show ParseTree where
  show = rec 0 where
    indent i = foldl (++) "" $ take i $ repeat "  "
    rec i tree = case tree of
      PT_Root   {             children = cs } -> foldl (\s c -> s++"\n"++(indent i)++" * "++(rec (i+1) c)) "" cs
      PT_Branch { pt_tag = t, children = cs } -> show t ++ foldl (\s c -> s++"\n"++(indent i)++" * "++(rec (i+1) c)) "" cs
      PT_LeafT  { pt_tag = t                } -> show t
      PT_LeafS  { pt_str = s                } -> s

parse_to_tree :: Specification -> ParseList -> ParseTree
parse_to_tree spec list = let

  rec :: ParseTree -> ParseList -> (ParseTree, ParseList)
  rec parent list = case list of
    []     -> (parent, [])
    (x:xs) -> case x of
      PLI_PTag pt ->
        let child = PT_LeafT { pt_tag = parsetag_to_tag spec pt }
        in if pt_is_end pt
          -- is an end tag
          then if pt_base_name spec pt == (tc_name.tag_class.pt_tag) parent
            -- does end the parent tag
            then (parent, xs)
            -- [!] is an end tag but does not end the parent tag
            else error_wrong_end pt parent
          -- is a new tag
          else if (env_is_container.tc_env.tag_class.pt_tag) child
            -- is a container
            then let
              (child', xs') = rec child xs
              parent' = add_child child' parent
              in rec parent' xs' 
            -- is not a container
            else let
              parent' = add_child child parent
              in rec parent' xs
      PLI_String s -> let
        child = PT_LeafS { pt_str = s }
        parent' = add_child child parent
        in rec parent' xs

  rec_root :: ParseTree -> ParseList -> (ParseTree, ParseList)
  rec_root root list =
    let (root', list') = rec root list
    in case list' of
      [] -> (root', [])
      _  -> let
        (root_, list'') = rec_root root list'
        root'' = concat_roots root' root_
        in (root'', list'') 

  in fst $ rec_root (PT_Root { children = [] }) list

error_wrong_args :: ParseTag -> TagClass -> a
error_wrong_args ptag tagclass = error $
  "incorrect number of arguments:" ++ "\n" ++
  "  tag-class `" ++ (tc_name tagclass) ++ "` expected `" ++ show (tc_arity tagclass) ++ "` arguments,"
  ++ "but found `" ++ show (length $ pt_args ptag) ++ "` arguments:" ++ "\n" ++
  "  " ++ show ptag

parsetag_to_tag :: Specification -> ParseTag -> Tag
parsetag_to_tag spec ptag@(PTag pt_name pt_is_end pt_args) =
  let mb_tagclass = get_by_key tc_name pt_name (tag_classes spec)
  in case mb_tagclass of
    Just tagclass ->
      if not $ check_arity (tc_arity tagclass) (length pt_args)
        -- [!] has incorrect number of arguments
        then error_wrong_args ptag tagclass
        -- has correct number of arguments
        else Tag tagclass pt_args

add_child :: ParseTree -> ParseTree -> ParseTree
add_child child parent = case parent of
  PT_Root     cs -> PT_Root     $ cs ++ [child]
  PT_Branch t cs -> PT_Branch t $ cs ++ [child]
  PT_LeafT  t    -> PT_Branch t $ [child]
  PT_LeafS  s    -> error $ "tried to add child to parent PT_LeafS: " ++ show (child, parent)

concat_roots :: ParseTree -> ParseTree -> ParseTree
concat_roots (PT_Root cs1) (PT_Root cs2) = PT_Root (cs1 ++ cs2)

error_wrong_end :: ParseTag -> ParseTree -> a
error_wrong_end pt parent = error $
  "expected `" ++ show pt ++ "` to be an end tag for the parent `" ++ show parent ++ "`"

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
