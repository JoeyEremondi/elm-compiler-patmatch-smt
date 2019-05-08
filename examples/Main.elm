type Foo = Bar | Baz

fooFun : Foo -> Int
fooFun x = case x of
  Bar -> 0
  Baz -> 1

fooFunApp = fooFun Baz

myMap : (a -> b) -> List a -> List b
myMap f l = case l of
  [] -> []
  (h :: t) -> (f h) :: (myMap f t) 