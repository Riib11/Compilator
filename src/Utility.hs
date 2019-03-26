module Utility where

begins :: String -> String -> Bool
begins target source = case (target, source) of
  ([]  , _   ) -> True
  (_   , []  ) -> False
  (t:t', s:s') -> if t == s then begins t' s' else False

wrap :: String -> [String]
wrap s = if s /= "" then [s] else []

extract :: String -> String -> Maybe String
extract target source =
  if target `begins` source then Just (drop len source) else Nothing
    where len = length target

extract_any :: [String] -> String -> Maybe (String, String)
extract_any targets source = case targets of
  []   -> Nothing
  t:ts -> case extract t source of { Nothing -> extract_any ts source; Just s' -> Just (t, s') }

extract_word :: [String] -> String -> (String, String)
extract_word splits "" = ("", "")
extract_word splits source@(c:s') =
  case extract_any splits source of
    Nothing      -> (c:word, s'') where (word, s'') = extract_word splits s'
    Just (x,s'') -> ("", source)
