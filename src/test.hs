main = do
  let x = if 0 == 1 then 1
          else if 0 == 2 then 2
          else if 0 == 3 then 3
          else if 0 == 4 then 4
          else if 0 == 0 then 0
  print x
