# Class FLIndices

`FLIndices` is a class that extends `list` through `FLlst` but
implements a set of features that give a little more structure to list
objects. The elements of `FLIndices` must all be of class `FLIndex`. It
implements a lock mechanism that, when turned on, does not allow the
user to increase or decrease the object length.

## Usage

``` r
FLIndices(object, ...)

# S4 method for class 'FLI'
FLIndices(object, ...)

# S4 method for class 'missing'
FLIndices(object, ...)

# S4 method for class 'list'
FLIndices(object, ...)
```

## Arguments

- object:

  unnamed object to be added to the list

- ...:

  other named or unnamed objects

## Slots

- .Data:

  The data. `list`.

- names:

  Names of the list elements. `character`.

- desc:

  Description of the object. `character`.

- lock:

  Lock mechanism, if turned on the length of the list can not be
  modified by adding or removing elements. `logical`.

## Constructor

A constructor method exists for this class that can take named arguments
for any of the list elements.

## See also

[FLlst](FLlst.md), [list](https://rdrr.io/r/base/list.html)

## Author

The FLR Team

## Examples

``` r
data(ple4.index)
flis <- FLIndices(INDa=ple4.index, INDb=window(ple4.index, end=2000))
```
