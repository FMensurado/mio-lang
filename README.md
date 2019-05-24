# mio-lang

toy language compile to mips

```
type:
  Unit
  Int
  Bool
  Float
  String

immutable:
let a: Int = 1

function:
fn fn1 a:Int b:Bool -> Int
  let m = bool_to_int b
  => a + m
```
