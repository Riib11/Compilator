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

data DefinitionValue = DV String | DV_Block  [(DefinitionAttribute, String)] deriving (Show) 

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

def_types =
  [ Def_Tag_Open, Def_Tag_Close, Def_Tag_ArgSep, Def_Tag_End,
    Def_Structure, Def_Defaults, Def_Section, Def_Tag ]

data DefinitionAttribute
  = DA_Item    String
  | DA_Section String
  | DA_Begin   String
  | DA_End     String
  | DA_Open    String
  | DA_Close   String
  | DA_Arity   String
  | DA_Arg     String
  deriving (Show)

compile_specification :: String -> Specification
compile_specification source = let

  rec :: String -> DefinitionDict
  rec s =
    let d = extract_def_header source
    in case d of
      Def_Tag_Open   -> (d, dv) : rec s' where (dv, s') = extract_dv d source
      Def_Tag_Close  -> (d, dv) : rec s' where (dv, s') = extract_dv d source
      Def_Tag_ArgSep -> (d, dv) : rec s' where (dv, s') = extract_dv d source
      Def_Tag_End    -> (d, dv) : rec s' where (dv, s') = extract_dv d source
      
      Def_Structure  -> (d, dv) : rec s' where (dv, s') = extract_dv_listblock d source
      
      Def_Defaults   -> (d, dv) : rec s' where (dv, s') = extract_dv_attrblock d source
      Def_Section    -> (d, dv) : rec s' where (dv, s') = extract_dv_attrblock d source
      Def_Tag        -> (d, dv) : rec s' where (dv, s') = extract_dv_attrblock d source

  dict_to_spec :: DefinitionDict -> Specification
  dict_to_spec = error "unimplemented"

  in rec source 

whitespace = [" ", "\n"]
rawstring_open = "___"
rawstring_close = "___"

extract_rawstring :: String -> (String, String)
extract_rawstring s = case extract_any whitespace s of
  Just s1 -> extract_rawstring s1
  Nothing -> case extract rawstring_open s of
    Nothing  -> error $ "expected rawstring open; found: " ++ s
    Just s2 -> case extract_word [rawstring_close] s2 of
      Nothing       -> error $ "expected rawstring close; found: " ++ s2
      Just (rs, s3) -> (rs, drop (length rawstring_close) s3)

extract_dv :: Definition -> String -> (DefinitionValue, String)
extract_dv def s = error "unimplemented"

extract_dv_listblock :: Definition -> String -> (DefinitionValue, String)
extract_dv_listblock def s = error "unimplemented"

extract_dv_attrblock :: Definition -> String -> (DefinitionValue, String)
extract_dv_attrblock def s = error "unimplemented"

header :: Definition -> String
header def = case def of
  Def_Tag_Open   -> "Tag-Open"
  Def_Tag_Close  -> "Tag-Close"
  Def_Tag_ArgSep -> "Tag-ArgSep"
  Def_Tag_End    -> "Tag-End"
  Def_Structure  -> "Structure"
  Def_Defaults   -> "Defaults"
  Def_Section    -> "Section"
  Def_Tag        -> "Tag"

extract_def_header :: String -> Definition
extract_def_header source =
  case get_by_keyfun (\s d -> header d `begins` s) def_types of
    Just d  -> drop (length header d)
    Nothing -> error "expected Definition header; found: " ++ source

