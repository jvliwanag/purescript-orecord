# purescript-orecord

A data type for records with required and optional values.

_Note: Documentations is WIP._

# Overview

A purescript record has a single row type identifying labels required to be present on a javascript object.
For instance, the type `Record ( i :: Int, s :: String )` only accepts record values having exactly two labels, `i` and `s`
referring to values of type `Int` and `String` respectively.

ORecord adds another row type to identify an addital set of labels which may or may not be present. A type
`ORecord ( i :: Int, s :: String ) ( b :: Boolean, n :: Number )` requires `i` and `s` to be present, but may or may not have
labels `b` and `n`.

In order to create an `ORecord`, we use the `o` which takes in a record:

```purescript
sample1 :: ORecord ( i :: Int, s :: String ) ( b :: Boolean, n :: Number )
sample1 = orecord { i: 10, s: "foo" }

sample2 :: ORecord ( i :: Int, s :: String ) ( b :: Boolean, n :: Number )
sample2 = orecord { i: 10, s: "foo", b: true }

sample3 :: ORecord ( i :: Int, s :: String ) ( b :: Boolean, n :: Number )
sample3 = orecord { i: 10, s: "foo", b: true, n: 2.1 }
```

It is important to note that the implementation of `o` coerces the record input to the proper `ORecord` type. This makes it especially useful when
working against JS FFI libraries defining record types with optional labels.
