module Specification where

import Utility
import Tag

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

data Specification = Specification
  
  { tag_open              :: String
  , tag_close             :: String
  , tag_argsep            :: String
  , tag_end               :: String
  
  , tag_classes           :: [TagClass]
  
  , tag_sections          :: [String]
  , default_section       :: String
  
  , compile_section_begin :: String -> String
  , compile_section_end   :: String -> String

  , compile_tag_open      :: Tag -> String
  , compile_tag_arg       :: Tag -> Int -> String -> String
  , compile_tag_close     :: Tag -> String

  , compile_tag_begin     :: Tag -> String
  , compile_tag_end       :: Tag -> String }


reserveds :: Specification -> [String]
reserveds spec = map (\f -> f spec) [tag_open, tag_close, tag_argsep]

word_splits :: Specification -> [String]
word_splits spec = [" "] ++ reserveds spec

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

type DefinitionDict = [(Definition, DefinitionValue)]

data DefinitionValue
  = DV_String    String
  | DV_AttrBlock [(DefinitionAttribute, String)]
  | DV_ListBlock [String]
  deriving (Show) 

data Definition
  = Def_Tag_Open
  | Def_Tag_Close
  | Def_Tag_ArgSep
  | Def_Tag_End
  | Def_Structure
  | Def_Defaults
  | Def_Section
  | Def_Tag
  deriving (Show)

def_header :: Definition -> String
def_header def = case def of
  Def_Tag_Open   -> "Tag-Open"
  Def_Tag_Close  -> "Tag-Close"
  Def_Tag_ArgSep -> "Tag-ArgSep"
  Def_Tag_End    -> "Tag-End"
  Def_Structure  -> "Structure"
  Def_Defaults   -> "Defaults"
  Def_Section    -> "Section"
  Def_Tag        -> "Tag"

def_types =
  [ Def_Tag_Open, Def_Tag_Close, Def_Tag_ArgSep, Def_Tag_End,
    Def_Structure, Def_Defaults, Def_Section, Def_Tag ]

data DefinitionAttribute
  = DA_Section
  | DA_Begin
  | DA_End
  | DA_Open
  | DA_Close
  | DA_Arity
  | DA_Arg
  deriving (Show)

defattr_types = [ DA_Section, DA_Begin, DA_End, DA_Open, DA_Close, DA_Arity, DA_Arg ]

defattr_header :: DefinitionAttribute -> String
defattr_header da = case da of
  DA_Section -> "section"
  DA_Begin   -> "begin"
  DA_End     -> "end"
  DA_Open    -> "open"
  DA_Close   -> "close"
  DA_Arity   -> "arity"
  DA_Arg     -> "arg"

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

compile_specification :: String -> Specification
compile_specification source = let
  dict = source_to_dict source
  spec = dict_to_spec   dict
  in spec

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

source_to_dict :: String -> DefinitionDict
source_to_dict s =
  let (d, s') = extract_def_header s
  in case d of
    Def_Tag_Open   -> (d, dv) : source_to_dict s'' where (dv, s'') = extract_dv d s'
    Def_Tag_Close  -> (d, dv) : source_to_dict s'' where (dv, s'') = extract_dv d s'
    Def_Tag_ArgSep -> (d, dv) : source_to_dict s'' where (dv, s'') = extract_dv d s'
    Def_Tag_End    -> (d, dv) : source_to_dict s'' where (dv, s'') = extract_dv d s'
    
    Def_Structure  -> (d, dv) : source_to_dict s'' where (dv, s'') = extract_dv_listblock s'
    
    Def_Defaults   -> (d, dv) : source_to_dict s'' where (dv, s'') = extract_dv_attrblock s'
    Def_Section    -> (d, dv) : source_to_dict s'' where (dv, s'') = extract_dv_attrblock s'
    Def_Tag        -> (d, dv) : source_to_dict s'' where (dv, s'') = extract_dv_attrblock s'

whitespace = [" ", "\n"]
rawstring_open = "___"
rawstring_close = "___"
block_open = "{"
block_close = "}"

extract_whitespace :: String -> String
extract_whitespace s = case extract_any whitespace s of { Nothing -> s; Just (_,s') -> s' }

extract_rawstring :: String -> (String, String)
extract_rawstring s =
  let s1 = extract_whitespace s
  in case extract rawstring_open s1 of
    Nothing  -> error $ "expected rawstring open; found: " ++ s1
    Just s2 -> case extract_till rawstring_close s2 of
      Nothing      -> error $ "expected rawstring close; found: " ++ s2
      Just (rs,s3) -> (rs, s3)

extract_dv :: Definition -> String -> (DefinitionValue, String)
extract_dv def s = case def of
  Def_Tag_Open   -> extract_dv_string s
  Def_Tag_Close  -> extract_dv_string s
  Def_Tag_ArgSep -> extract_dv_string s
  Def_Tag_End    -> extract_dv_string s
  
  Def_Structure  -> extract_dv_listblock s
  
  Def_Defaults   -> extract_dv_attrblock s
  Def_Section    -> extract_dv_attrblock s
  Def_Tag        -> extract_dv_attrblock s

extract_dv_string :: String -> (DefinitionValue, String)
extract_dv_string s = (DV_String v, s') where (v, s') = extract_rawstring s

extract_dv_attrblock :: String -> (DefinitionValue, String)
extract_dv_attrblock s = let
  extract_attrval :: String -> ((DefinitionAttribute, String), String)
  extract_attrval s = let
    s1       = extract_whitespace s
    (da, s2) = extract_defattr_header s1
    (v,  s3) = extract_rawstring s2
    in ((da, v), s3)
  rec :: String -> ([(DefinitionAttribute, String)], String)
  rec s = case extract block_close s of
    -- at end of block
    Just s' -> ([], s')
    -- get next attrval
    Nothing -> let
      (av,  s' ) = extract_attrval s
      (avs, s'') = rec s'
      in (av:avs, s'')
  (avs, s'') = rec s
  in (DV_AttrBlock avs, s'')

extract_dv_listblock :: String -> (DefinitionValue, String)
extract_dv_listblock s = let
  rec :: String -> ([String], String)
  rec s = let
    s1        = extract_whitespace s
    (v,  s')  = extract_rawstring s1
    (vs, s'') = rec s'
    in (v:vs, s'')
  (vs, s') = rec s
  in (DV_ListBlock vs, s')

extract_def_header :: String -> (Definition, String)
extract_def_header s =
  case get_by_keyfun (\s d -> def_header d `begins` s) s def_types of
    Just d  -> (d, drop (length $ def_header d) s)
    Nothing -> error $ "expected Definition header; found: " ++ s

extract_defattr_header :: String -> (DefinitionAttribute, String)
extract_defattr_header s =
  case get_by_keyfun (\s da -> defattr_header da `begins` s) s defattr_types of
    Just da -> (da, drop (length $ defattr_header da) s)
    Nothing -> error $ "expected DefinitionAttribute header; found: " ++ s


------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

dict_to_spec :: DefinitionDict -> Specification
dict_to_spec dict = error "unimplemented"
