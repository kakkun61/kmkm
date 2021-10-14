# kmkm

[![CI](https://github.com/kakkun61/kmkm/workflows/main/badge.svg)](https://github.com/kakkun61/kmkm/actions/workflows/main.yaml)

[![Sponsor](https://img.shields.io/badge/Sponsor-%E2%9D%A4-red?logo=GitHub)](https://github.com/sponsors/kakkun61)

## Notes

[A note about work in progress](https://zenn.dev/kakkun61/scraps/d45fad947b8e54) in Japanese.

## Language overview

### Hello world

```
(module
  main
  (list
    kmkm.io
    kmkm.prim)
  (list
    (bind-value
      main
      (procedure
        (list
          (call (apply kmkm.io.print "Hello world!")))))))
```

Instead this is a program that exits with code 1.

```
(module
  main
  (list
    kmkm.prim
    kmkm.proc)
  (list
    (bind-value
      main
      (procedure
        (list
          (call (apply kmkm.proc.pureInt 1)))))))
```

kmkm compiler translates this code into these C codes.

```c
#include "main.h"
#include "kmkm/prim.h"
#include "kmkm/proc.h"

kmkm_prim_int main(void)
{
  return kmkm_proc_pureInt(1);
}
```

```c
#ifndef MAIN_H
#define MAIN_H
#include "kmkm/prim.h"
#include "kmkm/proc.h"
kmkm_prim_int main(void);
#endif
```

### Comments

Not yet implemented.

### Identifiers

These identifiers are acceptable.

```
a
foo
fooBar
utf8
UTF8
```

Don't start with digit.

#### Qualified identifiers

These are valid qualified identifiers.

```
a.b.c
foo.bar
Foo.Bar
char.utf8
```

### Variable

Variables are introduced by let-expressions or functions' parameters.

Let-expressions have this form.

```
(let
  (list
    (bind-value foo 1))
  foo)
```

In this case `b` is bound to `1`.

### Modules

This module `foo.bar` imports no modules and defines no items.

```
(module
  foo.bar
  (list)
  (list))
```

Write module names at the first `list` when you want to import them.

```
(module
  foo.bar
  (list
    bar
    bar.buzz)
  (list))
```

Imported items are referenced with qualified names, for example `bar.buzz.foo`.

### Control flows

Not yet implemented.

### Primitive types

#### `kmkm.prim.int`

`kmkm.prim.int` is integer whose size is the same as `int` of C.

These represent three. `0b` is binary, `0o` is octal, and `0x` is hexadecimal.

```
3
0b11
0o3
0x3
```

#### `kmkm.prim.uint`

`kmkm.prim.uint` is unsigned integer whose size is the same as `unsigned int` of C.

Unsigned integer literal is not yet implemented.

#### `kmkm.prim.frac2`

`kmkm.prim.frac2` is double precision fraction number whose size is the same as `double` of C.

These represent approximately π.

```
3.1415926535
0x3.243f6a8822e87c199acbp0
```

These are hexadecimal fractions.

```
0x1p1
0x1p-1
0x1.1p1
```

`0x1p1` is _(1)₁₆ × 16¹_, `0x1p-1` is _(1)₁₆ × 16⁻¹_, and `0x1.1p1` is _(1.1)₁₆ × 16¹_.

#### `kmkm.prim.string`

```
"hello"
"こんにちは"
"""triple-double-quoted strings can contain "double quotes""""
"escaping \\"
```

### Functions

#### Function application

This code is an example of a function application.

```
(apply (apply kmkm.int.add 1) 2)
```

`kmkm.int.add` is typed to `function kmkm.prim.int (function kmkm.prim.int kmkm.prim.int))`.

#### Function definition

This `add` takes two `kmkm.prim.int` arguments and returns `kmkm.prim.int`.

```
(bind-value
  add
  (function a kmkm.prim.int (function b kmkm.prim.int (apply (apply kmkm.int.add a) b))))
```

This `succ` takes one `kmkm.prim.int` arguments and return `kmkm.prim.int`.

```
(bind-value
  succ
  (apply func.add 1))
```

### Procedures

The procedure is a value with side effects.

For instance `printInt` has `(function kmkm.prim.int (procedure kmkm.prim.int))` as its type.

Procedures can be called in other procedures with `call` and `bind` keywords.

```
(module
  main
  (list
    kmkm.prim
    kmkm.int
    kmkm.io
    kmkm.proc)
  (list
    (bind-value
      main
      (let
        (list
          (bind-value a (apply (apply kmkm.int.add 1) 2)))
        (procedure
          (list
            (call (apply kmkm.io.printInt a))
            (call (apply kmkm.proc.pureInt a))))))))
```

`main.main` is typed in `(procedure kmkm.prim.int)`.

### Foreign function interfaces

#### Values

`add` is implemented as a C function.

```
(bind-value-foreign
  add
  (list
    (c-value
      ""
      (list "a" "b")
      "return a + b;"))
  (function kmkm.prim.int (function kmkm.prim.int kmkm.prim.int)))))
```

When header files are “include”-d, put them at the first parameter of `c-value`.

```
(c-value
  "#include <stdio.h>"
  …
```

#### Types

`int` is “typedef”-ed as C `int`;

```
(bind-type-foreign
  int
  (list (c-type "" "int")))
```

When header files are “include”-d, put them at the first parameter of `c-type`.

```
(c-type
  "#include <stdio.h>"
  …
```
