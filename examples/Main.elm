-- type Foo = Bar | Baz
-- 
-- fooFun : Foo -> Int
-- fooFun x = case x of
--   Bar -> 0
--   Baz -> 1
-- 
-- fooFunApp = fooFun Baz
-- 
-- myMap : (a -> b) -> List a -> List b
-- myMap f l = case l of
--   [] -> []
--   (h :: t) -> (f h) :: (myMap f t) 

--mapLast : (a -> a) -> List a -> List a
--mapLast update list =
--    case list of
--        [] ->
--            list
--
--        --only :: [] ->
--        --    [ update only ]
--
--        --first :: rest ->
--        --    first :: mapLast update rest

foo : (List a) -> List a
foo list = 
  case list of
    [] -> list
    (only :: []) -> list