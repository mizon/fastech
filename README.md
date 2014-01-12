# Fastech - A fast parser combinator library for Common Lisp

WARNING: This project is in development now!

Fastech is a fast and pragmatic parser combinator library for Common Lisp.

The concept and the implementation are inspired by [Attoparsec][].

[Attoparsec]: https://github.com/bos/attoparsec

## Features

* CPS-based fast implementation
* Includes string specific efficient parsers like `take-while`
* [TODO] Includes high level useful combinators
* [TODO] Supports incremental inputs which is useful to parse streams

## Installation

Clone this repository and load `fastech.asd`. After loading it, run the below command.

```common-lisp
(asdf:load-system :fastech)
```

You may get some errors about library dependence. If so, please install the required libraries and retry the above command.

## Usage

At first, whole APIs Fastech provides are exported from `fastech` package. Import them as needed.

### Running parsers

Use `fastech:parse` and `fastech:parse-only` to run your parser. `parse` is used for incremental parsing, so `parse-only` may be convenient for daily use. For example:

```common-lisp
(fastech:parse-only (fastech:str "foo") "foobar")
;=> "foo" "bar"
```

### Composing parsers

Use `fastech:bind`, `fastech:*>` or `fastech:<*`.

#### fastech:bind

`fastech:bind` takes a parser and a function which takes first parser's result and returns second parser. The bound parser applies those two parsers sequencially.

```common-lisp
(parse-only (bind (str "foobar")
                  (lambda (v)
                    (always v)))
            "foobar")
;=> "foobar"
```

#### fastech:\*>, fastech:<\*

`fastech:*>` and `fastech:<*` take more than one parsers and apply given parsers sequencially, but `*>` only keeps the last parser's result and `<*` only keeps the first parser's result.

```common-lisp
(parse-only (*> (chr #\a) (chr #\b) (chr #\c)) "abc")
;=> "c"
(parse-only (<* (chr #\a) (chr #\b) (chr #\c)) "abc")
;=> "a"
```

### Back tracking

Use `fastech:try`. This keeps the input position and rewind it if the inner parser failed.

```common-lisp
(try (*> (str "foo") (str "foo")) "foobar")
;=> parse-error: remainder: "foobar"
```

However, take care of using `try` because it may make your parser slowly.

## For performance

For writing efficient parsers, prefer to use string specific parsers like `fastech:str`, `fastech:take-while`, `fastech:take-till`, and the others. These parsers are truly faster than composed character specific parsers like `(fastech:many (fastech:any-char))`.

## Limitation

Fastech now be tested with SBCL and Clozure CL only. I will support other Lisp implementations in the future.

## License

[BSD3](http://opensource.org/licenses/BSD-3-Clause)
