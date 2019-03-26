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
