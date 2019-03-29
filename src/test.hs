data X = X Int Int Int

set :: Int -> X -> X
set i
  (X a b c) =
  (X a b i)
