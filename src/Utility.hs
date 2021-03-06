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

extract_till :: String -> String -> Maybe (String, String)
extract_till split source =
  case source of
    ""   -> Nothing
    c:s' -> case extract split source of
      Nothing -> fmap f (extract_till split s') where f (word, s'') = (c:word, s'')
      Just s' -> Just ("", s')

get_by_key :: Eq b => (a -> b) -> b -> [a] -> Maybe a
get_by_key key_of x = foldl (\mb_a a -> if key_of a == x then Just a else mb_a) Nothing

get_by_keyfun :: (a -> b -> Bool) -> a -> [b] -> Maybe b
get_by_keyfun eq a = foldl (\mb_b b -> if eq a b then Just b else mb_b) Nothing
