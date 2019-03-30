module SpecificationCompiler where

import Control.Lens
import Specification
import Utility
import Tag

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

compile_specification :: String -> Specification
compile_specification source = dict_to_spec . source_to_dict $ source

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

type SpecificationDictionary = [SD_Item]

data SD_Item
  = SDI_Tag_Open   String
  | SDI_Tag_Close  String
  | SDI_Tag_ArgSep String
  | SDI_Tag_End    String
  | SDI_Structure  [String]
  | SDI_Section
    { _sd_sec_begin   :: String
    , _sd_sec_end     :: String }
  | SDI_Defaults
    { _sd_dft_section :: String
    , _sd_dtf_begin   :: String
    , _sd_dtf_end     :: String
    , _sd_dtf_open    :: String
    , _sd_dtf_close   :: String }
  | SDI_Tag
    { _sd_tag_section :: String
    , _sd_tag_begin   :: String
    , _sd_tag_end     :: String
    , _sd_tag_open    :: String
    , _sd_tag_close   :: String
    , _sd_tag_arity   :: Arity
    , _sd_tag_args    :: [TagArg] }


source_to_dict :: String -> SpecificationDictionary
source_to_dict s = let
  s1 = extract_whitespace s
  (sdi, s'') = case extract_any sdi_headers s1 of
    Just ("Tag-Open",   s') -> extract_tag_open   s'
    Just ("Tag-Close",  s') -> extract_tag_close  s'
    Just ("Tag-ArgSep", s') -> extract_tag_argsep s'
    Just ("Tag-End",    s') -> extract_tag_end    s'
    Just ("Structure",  s') -> extract_structure  s'
    Just ("Defaults",   s') -> extract_defaults   s'
    Just ("Section",    s') -> extract_section    s'
    Just ("Tag",        s') -> extract_tag        s'
    Nothing -> error $ "no header found at: " ++  s1
  in sdi : source_to_dict s'

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

sdi_headers = ["Tag-Open", "Tag-Close", "Tag-ArgSep", "Tag-End", "Structure", "Defaults", "Section" , "Tag"]

------------------------------------------------------------------------------------------------------------------------------

extract_tag_open s = extract_sdi_string SDI_Tag_Open
extract_tag_close  = extract_sdi_string SDI_Tag_Close
extract_tag_argsep = extract_sdi_string SDI_Tag_ArgSep
extract_tag_end    = extract_sdi_string SDI_Tag_End

extract_sdi_string :: (String -> SDI_Item) -> String -> (SDI_Item, String)
extract_sdi_string construct s =
  let (v, s') = extract_rawstring . extract_whitespace s
  in (construct v, s')

------------------------------------------------------------------------------------------------------------------------------

extract_structure :: String -> (SDI_Item, String)
extract_structure s = let
  rec :: String -> ([String], String)
  rec s = case extract block_close s of
    Just s' -> ([], s')
    Nothing -> let
      (v,  s' ) = extract_rawstring . extract_whitespace s
      (vs, s'') = rec s'
      in (v:vs, s'')
  (vs, s') = rec s
  in (SDI_Structure vs, s')

------------------------------------------------------------------------------------------------------------------------------

sdi_section_headers  = ["begin", "end"]
extract_section

------------------------------------------------------------------------------------------------------------------------------

extract_defaults
sdi_defaults_headers = ["begin", "end", "open", "close", "section"]

------------------------------------------------------------------------------------------------------------------------------

extract_tag
sdi_tag_headers      = ["begin", "end", "open", "close", "section", "arity", "arg"]

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

whitespace      = [" ", "\n"]
rawstring_open  = "___"
rawstring_close = "___"
block_open      = "{"
block_close     = "}"

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

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

dict_to_spec :: SpecificationDictionary -> Specification
dict_to_spec dict = error "unimplemented"


------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

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
