type Foo = Bar | Baz

f : Foo -> Int
f x = case x of
  Bar -> 0

g = f Baz