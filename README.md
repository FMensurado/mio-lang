# mio-lang

toy language compile to mips

```
type:
  Unit
  Int
  Bool
  Float
  String

Nested Type:
  Union Type: Unit | Int
  Tuple Type: (Unit, Int)
  Ref Type
  Record Type

immutable:
a: Int = 1

ref:
a: Ref<Int> = ref 1
b = deref a
a = ref 2

record:
Record Point = {
  x: Int,
  y: Int,
}
point: Pint = { x = 0, y = 0 }

function:
fn1 a:Int b:Bool -> Int = a + (Types.boolToInt a)

overload:
fn1 a:Int b:Bool -> Int  = a + (Types.boolToInt a)
fn1 a:Int b:Float -> Int = a + (Types.floatToInt a)

generetic:
fn1 [T := String, U := String] a:T b:U -> Unit =
  Console.log(a)
  Console.log(b)
```
