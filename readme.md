# A Simple Go Macro Implementation

**go\_macro is a simple, lightweight go macro implementation written by Rust.** 
It provides macro definition and invocation feature for go. 
You can use the macro as Generics implementation, hyper-function, 
or anything you can do with macros(as you like). 
The macro is inspired by the C preprocessor.

## Usage

Download the command-line binary `go-macro`, and then run:

```shell
go-macro --source <go file or directory> --target <target directory> [--format]
```

The program would translate all input go files into target directory, 
and the `--format` option represents whether to format the code by `gofmt`(binary required). 
The default `--source` and `--default` option are `./` and `./go-target`. 
Then you can run `cd <target directory> && go build` to build your go program. 
See the example in `example` directory.

## Syntax

### \#define

You can define a macro by using `#define`, and ends a definition by using `#enddef`: 

```go
#define MAX_CPU_NUM
    4
#enddef
```

or with arguments:

```go
#define Wrapper(T)
    struct{ Inner T }
#enddef
```

The macro can be used anywhere, just like a variable or a function:

```go
const ProcessorNum = MAX_CPU_NUM
```

will be translated to:

```go
const ProcessorNum = 4
```

and

```go
var wrapper = Wrapper(int){ Inner: 0 }
```

will be translated to

```go
var wrapper = struct{ Inner int }{ Inner: 0 }
```

### \#include

You can include macros in other files by using `#include "<file/directory>"`:

```go
#include "./macros"
```

All macros in "./macros" will be imported.

**NOTE: the program would panic if cross reference happens.**

## Builtin Macros

There are two builtin macros: `concat`/`str`.

1. The `concat` macro is to concat the given arguments;
2. The `str` macro converts input as a `string`;

see the example below:

```go
#define NUM
    str(3)
#enddef

func main() {
    // here the i shoud be "3"
    var i = concat(N, U, M)
}
```

## Recursion Limit

The macro can be invoked recursively:

```go
#define TYPE
    int64
#enddef

#define add
    func(x, y TYPE) TYPE {
        return x + y
    }
#enddef

func main() {
    var _ = add(1, 2)
}
```

will be translate to

```go
func main() {
    var _ = func(x, y int64) int64 {
        return x + y
    }(1, 2)
}
```

The code below would cause a recursion limit error:

```go
#define name 
    name
#enddef
```

the recursion limit is 32 by default.

## Example

Let's make Generics possible by using macro:

```go
#define sort(T)
    func(list []T) {
        // sort implement here
    }
#enddef

func main() {
    var list = []byte("string")
    sort(byte)(list)

    // would be "ginrst"
    fmt.Println(string(list))
}
```

## Todo
- [x] Add command `#include`;
- [ ] Add more commands like `#if`, `#for`; 
- [ ] Use LinkedList instead of Vec for better performance;
- [ ] Maybe it is necessary to write more comments?

