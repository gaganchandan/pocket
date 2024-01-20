# pocket
Pocket is a minimal, statically typed, procedural programming language.

## Features
Pocket has a C-like syntax and execution starts at the `main` function. It supports basic I/O operations and provides constructs for conditional and iterative execution. It also supports user defined functions. The data types provided are Booleans, characters and integers. There are also one-dimensional lists of these types and strings are simply character lists. As of now, type annotations are mandatory but type inference is a planned addition.

## Getting started
```
git clone https://github.com/gaganchandan/pocket
cd pocket
dune build --release
dune exec --release pocket <program>
```

## Examples
 Here is a "Hello, World!" program in Pocket:

```
function main() : none {
    print("Hello, World\n");
}
```

A more complex program that prints the first 10 Fibonacci numbers:

```
function main() : none {
    a : int = 0;
    b : int = 1;
    c : int = 0;
    i : int = 0;
    while (i < 10) {
        print(a);
        print(" ");
        c = a + b;
        a = b;
        b = c;
        i = i + 1;
    }
    print("\n");
}
```


The `examples` directory contains sample programs that demonstrate most language features.


## Name
The name is a reference to the fact that the language is so small, it can fit in your pocket!
