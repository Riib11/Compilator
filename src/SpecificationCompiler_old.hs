module SpecificationCompiler where

import Specification
import Utility
import Tag

type DefinitionDict = [(Definition, DefinitionValue)]

data Definition
  = Def_Tag_Open
  | Def_Tag_Close
  | Def_Tag_ArgSep
  | Def_Tag_End
  | Def_Structure
  | Def_Defaults
  | Def_Section
  | Def_Tag
  deriving (Show, Eq)

data DefinitionValue
  = DV_String    String
  | DV_AttrBlock DefinitionAttributeDict
  | DV_ListBlock [String]
  deriving (Show, Eq)

type DefinitionAttributeDict = [(DefinitionAttribute, String)]

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
  = DA_Name
  | DA_Section
  | DA_Begin
  | DA_End
  | DA_Open
  | DA_Close
  | DA_Arity
  | DA_Arg
  deriving (Show, Eq)

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

get_attrval :: DefinitionAttribute -> DefinitionAttributeDict -> String
get_attrval da dict = case dict of
  [] -> error $ "could not find" ++ show da ++ " in " ++ show dict
  (da',v): dict' -> if da == da' then v else get_attrval da dict'

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

compile_specification :: String -> Specification
compile_specification source = dict_to_spec . source_to_dict $ source

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
  Def_Tag        -> extract_tagblock s

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
  
  rec :: String -> (DefinitionAttributeDict, String)
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

extract_tagblock :: String -> (DefinitionValue, String)
extract_tagblock s = let
  (tc, s' ) = extract_rawstring s
  (DV_AttrBlock avs, s'') = extract_dv_attrblock s'
  attrblock = DV_AttrBlock $ (DA_Name, tc) : avs
  in (attrblock, s'')

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
dict_to_spec dict = let
  rec :: DefinitionDict -> Specification -> Specification
  rec dict spec = case dict of
    defaults = 
    [] -> spec
    ((def, dv):dict') -> let
      update = case (def, dv) of
        (Def_Tag_Open,   DV_String v) -> set_tag_open v
        (Def_Tag_Close,  DV_String v) -> set_tag_close v
        (Def_Tag_ArgSep, DV_String v) -> set_tag_argsep v
        (Def_Tag_End,    DV_String v) -> set_tag_end v
        
        (Def_Structure, DV_ListBlock vs) -> set_tag_sections vs
        
        (Def_Defaults, DV_AttrBlock avs) -> set_default_section (get_attrval DA_Section avs)
        (Def_Section,  DV_AttrBlock avs) ->
          add_compile_section_begin (get_attrval DA_Section avs, get_attrval DA_Begin avs) .
          add_compile_section_end   (get_attrval DA_Section avs, get_attrval DA_End avs)
        (Def_Tag,      DV_AttrBlock avs) ->
          add_compile_tag_open
          add_compile_tag_close
          add_compile_ 
          add_compile_tag_begin (get_attrval DA_Name avs, get_attrval) .
      spec' = update spec
      in rec dict' spec'

  in rec dict init_spec

error_no_spec :: String -> a
error_no_spec s = error $ "not specified: " ++ s

init_spec :: Specification
init_spec = Specification
  { _tag_open              = error_no_spec $ def_header Def_Tag_Open
  , _tag_close             = error_no_spec $ def_header Def_Tag_Close
  , _tag_argsep            = error_no_spec $ def_header Def_Tag_ArgSep
  , _tag_end               = error_no_spec $ def_header Def_Tag_End
  , _tag_classes           = []
  , _sections              = []
  , _compile_section_begin = \s -> ""
  , _compile_section_end   = \s -> ""
  , _compile_tag_open      = \t -> ""
  , _compile_tag_arg       = \t i v -> " " ++ v ++ " "
  , _compile_tag_close     = \t -> ""
  , _compile_tag_begin     = \t -> ""
  , _compile_tag_end       = \t -> ""

  , _dft_section           = error_no_spec "Defaults.section"
  , _dft_compile_tag_open  = \t   -> error_no_spec $ "Defaults.open; found: "  ++ show t
  , _dft_compile_tag_arg   = \t i -> error_no_spec $ "Defaults.arg; found: "   ++ show (t, i)
  , _dft_compile_tag_close = \t   -> error_no_spec $ "Defaults.close; found: " ++ show t
  , _dft_compile_tag_begin = \t   -> error_no_spec $ "Defaults.begin; found: " ++ show t
  , _dft_compile_tag_end   = \t   -> error_no_spec $ "Defaults.end; found: "   ++ show t

}
