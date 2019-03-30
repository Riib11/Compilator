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
    { _sdi_sec_name    :: String
    , _sdi_sec_begin   :: String
    , _sdi_sec_end     :: String }
  | SDI_Defaults
    { _sdi_dft_section :: String
    , _sdi_dtf_begin   :: String
    , _sdi_dtf_end     :: String
    , _sdi_dtf_open    :: String
    , _sdi_dtf_close   :: String }
  | SDI_Tag
    { _sdi_tag_name    :: String
    , _sdi_tag_section :: String
    , _sdi_tag_begin   :: String
    , _sdi_tag_end     :: String
    , _sdi_tag_open    :: String
    , _sdi_tag_close   :: String
    , _sdi_tag_arity   :: Arity
    , _sdi_tag_args    :: TagArgs }


source_to_dict :: String -> SpecificationDictionary
source_to_dict s = let
  s1 = extract_whitespace s
  (sdi, s') = extract_sdi s1
  in if s == "" then [] else sdi : source_to_dict s'

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

sdi_headers = ["Tag-Open", "Tag-Close", "Tag-ArgSep", "Tag-End", "Structure", "Defaults", "Section" , "Tag"]

extract_sdi s = case extract_any sdi_headers s of
  Just ("Tag-Open",   s') -> extract_tag_open   s'
  Just ("Tag-Close",  s') -> extract_tag_close  s'
  Just ("Tag-ArgSep", s') -> extract_tag_argsep s'
  Just ("Tag-End",    s') -> extract_tag_end    s'
  Just ("Structure",  s') -> extract_structure  s'
  Just ("Defaults",   s') -> extract_defaults   s'
  Just ("Section",    s') -> extract_section    s'
  Just ("Tag",        s') -> extract_tag        s'
  Nothing -> error $ "no header found at: " ++  s1

------------------------------------------------------------------------------------------------------------------------------

extract_tag_open s = extract_sdi_string SDI_Tag_Open
extract_tag_close  = extract_sdi_string SDI_Tag_Close
extract_tag_argsep = extract_sdi_string SDI_Tag_ArgSep
extract_tag_end    = extract_sdi_string SDI_Tag_End

extract_sdi_string :: (String -> SD_Item) -> String -> (SD_Item, String)
extract_sdi_string construct s =
  let (v, s') = extract_rawstring . extract_whitespace s
  in (construct v, s')

------------------------------------------------------------------------------------------------------------------------------

extract_structure :: String -> (SD_Item, String)
extract_structure s = let
  rec :: String -> ([String], String)
  rec s =
    let s1 = extract_whitespace s
    in case extract block_close s1 of
      Just s' -> ([], s')
      Nothing -> let
        (v,  s' ) = extract_rawstring . s1
        (vs, s'') = rec s'
        in (v:vs, s'')
  (vs, s') = rec s
  in (SDI_Structure vs, s')

------------------------------------------------------------------------------------------------------------------------------

sdi_section_headers = ["begin", "end"]

extract_section :: String -> (SD_Item, String)
extract_section s = let
  
  -- name
  (name, s') = extract_rawstring s
  -- block open
  s'' = case extract block_open s' of
    Nothing  -> error $ "expected block_close; found: " ++ s'
    Just s'' -> s''

  sdi_sec_init = SDI_Section
    { _sdi_sec_name  = name
    , _sdi_sec_begin = error_no_spec $ "Section.begin for " ++ name
    , _sdi_sec_end   = error_no_spec $ "Section.end for "   ++ name }

  rec :: String -> SD_Item -> (SD_Item, String)
  rec s sdi_sec =
    let s1 = extract_whitespace s
    in case extract block_close s1 of
      Just s' -> (sdi_sec, s')
      Nothing -> case extract_any sdi_section_headers s1 of
        Nothing -> error $ "expected section header; found: " ++ s1
        Just (h,s') -> let
          (v, s'') = extract_rawstring s'
          sdi_sec' = set (sec_header_to_field h) v sdi_sec
          in rec s'' sdi_sec'
  
  in rec s' sdi_sec_init

sec_header_to_field s = case s of
  "begin" -> sd_sec_begin
  "end"   -> sd_sec_end

------------------------------------------------------------------------------------------------------------------------------

sdi_defaults_headers = ["begin", "end", "open", "close", "section"]

extract_defaults :: String -> (SD_Item, String)
extract_defaults s = let
  
  s1 = extract_whitespace s
  -- block open
  s' = case extract block_open s1 of
    Nothing  -> error $ "expected block_close; found: " ++ s1
    Just s' -> s'

  sdi_defaults_init = SDI_Defaults
    { _sdi_dft_section = error_no_spec $ "Defaults.section"
    , _sdi_dtf_open    = error_no_spec $ "Defaults.open"
    , _sdi_dtf_close   = error_no_spec $ "Defaults.close"
    , _sdi_dtf_begin   = error_no_spec $ "Defaults.begin"
    , _sdi_dtf_end     = error_no_spec $ "Defaults.end" }

  rec :: String -> SD_Item -> (SD_Item, String)
  rec s sdi_dft =
    let s1 = extract_whitespace s
    in case extract block_close s1 of
      Just s' -> (sdi_dft, s')
      Nothing -> case extract_any sdi_defaults_headers s1 of
        Nothing     -> error $ "expected Defaults header; found: " ++ s1
        Just (h,s') -> let
          (v, s'') = extract_rawstring s'
          sdi_dft' = set (dft_header_to_field h) v sdi_dft
          in rec s'' sdi_dft'

  in rec s' sdi_defaults_init

dst_header_to_field h = case h of
  "begin"   -> sdi_dtf_begin
  "end"     -> sdi_dft_end
  "close"   -> sdi_dft_close
  "open"    -> sdi_dft_open
  "section" -> sdi_dft_section

------------------------------------------------------------------------------------------------------------------------------

sdi_tag_headers = ["begin", "end", "open", "close", "section", "arity", "arg"]

extract_tag :: String -> (SD_Item, String)
extract_tag s = let

  sdi_tag_init = SDI_Tag
    {}

  rec :: String -> SD_Item -> (SD_Item, String)
  rec s sdi_tag =
    let s1 = extract_whitespace s
    in case extract block_close s1 of
      Just s' -> (sdi_tag, s')
      Nothing -> case extract_any sdi_tag_headers s1 of
        Nothing     -> error $ "Expected Tag header; found: " ++ s1
        Just (h,s') -> let
          (v, s'') = case h of
            "arity" -> extract_tag_arity s'
            "arg"   -> extract_tag_arg   s' (view sdi_tag_args sdi_tag)
            _       -> extract_rawstring s'
          sdi_tag' = set (tag_header_to_field h) v sdi_tag
          in rec s'' sdi_tag'

   in rec s' sdi_tag_init


extract_tag_arity :: String -> Arity
extract_tag_arity s = error $ "unimplemented"


data TagArgs = TagArgs {}
extract_tag_arg :: String -> TagArgs -> TagArgs
extract_tag_arg s = error $ "unimplemented"

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
