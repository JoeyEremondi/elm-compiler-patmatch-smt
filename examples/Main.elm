f : Int -> Int
f x = 
  let y = x + 1 in
  let z = y * 2 in
  let g w = w + z in
  x + y + z 