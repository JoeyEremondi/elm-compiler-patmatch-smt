f : Bool -> Int -> Int
f b x = 
  let y = x + 1 in
  let z = y * 2 in
  let g w = w + z in
  x + y + z 