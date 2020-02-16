let foo a b c =
  let rec f x = if x < 0 then 42 else a + g x (x + 1)
      and g y y' = a + b + h (y + y' - 1, y)
      and h (z, z') = c + f z + f z'
  in
  f
