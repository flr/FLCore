# Class FLModelSims

A list of [`FLModelSim`](FLModelSim.md) objects.

## Usage

``` r
FLModelSims(object, ...)

# S4 method for class 'ANY'
FLModelSims(object, ...)

# S4 method for class 'missing'
FLModelSims(object, ...)

# S4 method for class 'list'
FLModelSims(object)

# S4 method for class 'FLModelSims'
FLModelSims(object)
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

[FLlst](FLlst.md), [list](https://rdrr.io/r/base/list.html),
[vector](https://rdrr.io/r/base/vector.html)

## Author

The FLR Team
