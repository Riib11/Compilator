module Compiler where

import Utility
import Tag
import Specification
import Parser

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

compile :: Specification -> ParseTree -> String
compile spec = compile_to_target spec . compile_to_document spec

compileIO :: Specification -> ParseTree -> IO String
compileIO spec tree = do
  let doc = compile_to_document spec tree
  putStrLn $ "document:\n" ++ show doc
  let tar = compile_to_target spec doc
  putStrLn $ "target-text:\n" ++ tar
  return tar

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

data Document = Document
  { doc_sections :: [(String, [ParseTree])]
  }

instance Show Document where
  show (Document { doc_sections = secs }) =
    foldl (++) "" $ map f secs where
      f (sec, trees) = sec ++ ":" ++ "\n" ++ foldl (\s t -> s ++ "* " ++ show t ++ "\n") "" trees

compile_to_document :: Specification -> ParseTree -> Document
compile_to_document spec (PT_Root { children = cs }) = let
  
  rec :: [ParseTree] -> Document -> Document
  rec trees doc =
    case trees of
      [] -> doc
      tree:trees' -> let
        doc' = case tree of
          PT_Branch { pt_tag = t } -> add_to_section tree (tc_sec.tag_class $ t)     doc
          PT_LeafT  { pt_tag = t } -> add_to_section tree (tc_sec.tag_class $ t)     doc
          PT_LeafS  { pt_str = s } -> add_to_section tree (view dft_section spec) doc
        in rec trees' doc'

  init_doc_sections secs = case secs of { [] -> []; (s:secs') -> (s, []) : init_doc_sections secs' }

  in rec cs (Document { doc_sections = init_doc_sections (view tag_sections spec) })

add_to_section :: ParseTree -> String -> Document -> Document
add_to_section t sec_tar (Document { doc_sections = sec_tss }) =
  Document { doc_sections = map f sec_tss } where
    f (sec, ts) = if sec == sec_tar then (sec, ts++[t]) else (sec, ts)

------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------

compile_to_target :: Specification -> Document -> String
compile_to_target spec doc = let

  compile_tag :: Tag -> String
  compile_tag t =
    view compile_tag_open spec t ++
    (foldl (++) "" $ map (\(i, a) -> view compile_tag_arg spec t i a) (zip [1..] $ tag_args t)) ++
    view compile_tag_close spec t

  compile_tree :: ParseTree -> String
  compile_tree tree = case tree of
    PT_Branch { pt_tag = t, children = cs } ->
      if (env_is_container.tc_env.tag_class $ t)
        then
          view compile_tag_begin spec t ++
          compile_tag t ++
          foldr (\c s -> compile_tree c ++ s) "" cs ++
          view compile_tag_end spec t
        else
          foldr (\c s -> compile_tree c ++ s) "" cs
    PT_LeafT { pt_tag = t } -> compile_tag t
    PT_LeafS { pt_str = s } -> s

  compile_section :: (String, [ParseTree]) -> String
  compile_section (sec, ts) =
    view compile_section_begin spec sec ++
    foldr (\t s -> compile_tree t ++ s) "" ts ++
    view compile_section_end spec sec
  
  rec :: [(String, [ParseTree])] -> String
  rec secs = case secs of { [] -> ""; sec:secs' -> compile_section sec ++ rec secs' }

  in rec $ doc_sections doc
