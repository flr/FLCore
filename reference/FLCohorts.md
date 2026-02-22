# Class FLCohorts

`FLCohorts` is a class that extends `list` through `FLlst` but
implements a set of features that give a little more structure to list
objects. The elements of `FLCohorts` must all be of class `FLCohort`. It
implements a lock mechanism that, when turned on, does not allow the
user to increase or decrease the object length.

## Usage

``` r
FLCohorts(object, ...)
```

## Arguments

- object:

  unnamed object to be added to the list

- ...:

  other named or unnamed objects

## Slots

- .Data:

  The data. `list`

- names:

  Names of the list elements. `character`

- desc:

  Description of the object. `character`

- lock:

  Lock mechanism, if turned on the length of the list can not be
  modified by adding or removing elements. `logical`

## Constructor

A constructor method exists for this class that can take named arguments
for any of the list elements.

## See also

[\*](https://rdrr.io/r/base/Arithmetic.html),
[Arith](https://rdrr.io/r/methods/S4groupGeneric.html),
[as.data.frame](https://rdrr.io/r/base/as.data.frame.html),
[bubbles](bubbles.md), catch\<-, [iter](iter.md),
[model.frame](https://rdrr.io/r/stats/model.frame.html),
[show](https://rdrr.io/r/methods/show.html),
[summary](https://rdrr.io/r/base/summary.html),
[xyplot](https://rdrr.io/pkg/lattice/man/xyplot.html),
[FLlst](FLlst.md), [list](https://rdrr.io/r/base/list.html)

## Author

The FLR Team
