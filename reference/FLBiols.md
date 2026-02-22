# Class FLBiols

A list of `FLBiol` objects.

## Usage

``` r
FLBiols(object, ...)

# S4 method for class 'FLBiol'
FLBiols(object, ...)

# S4 method for class 'missing'
FLBiols(object, ...)

# S4 method for class 'list'
FLBiols(object, ...)
```

## Arguments

- object:

  unnamed object to be added to the list

- ...:

  other named or unnamed objects

## Slots

- .Data:

  Internal S4 data representation, of class `list`.

- desc:

  As textual description of the object contents

- lock:

  Can the object be extended/trimmed? `TRUE` or `FALSE`.

- names:

  A character vector for the element names

## Constructor

A constructor method exists for this class that can take named arguments
for any of the list elements.

## See also

[`FLlst`](FLlst.md), [`list`](https://rdrr.io/r/base/list.html),
[`vector`](https://rdrr.io/r/base/vector.html)

## Author

The FLR Team
