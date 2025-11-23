<div id="main" class="col-md-9" role="main">

# Class FLIndices

<div class="ref-description section level2">

`FLIndices` is a class that extends `list` through `FLlst` but
implements a set of features that give a little more structure to list
objects. The elements of `FLIndices` must all be of class `FLIndex`. It
implements a lock mechanism that, when turned on, does not allow the
user to increase or decrease the object length.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
FLIndices(object, ...)

# S4 method for class 'FLI'
FLIndices(object, ...)

# S4 method for class 'missing'
FLIndices(object, ...)

# S4 method for class 'list'
FLIndices(object, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    unnamed object to be added to the list

-   ...:

    other named or unnamed objects

</div>

<div class="section level2">

## Slots

-   .Data:

    The data. `list`.

-   names:

    Names of the list elements. `character`.

-   desc:

    Description of the object. `character`.

-   lock:

    Lock mechanism, if turned on the length of the list can not be
    modified by adding or removing elements. `logical`.

</div>

<div class="section level2">

## Constructor

A constructor method exists for this class that can take named arguments
for any of the list elements.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLlst](FLlst.md), [list](https://rdrr.io/r/base/list.html)

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data(ple4.index)
flis <- FLIndices(INDa=ple4.index, INDb=window(ple4.index, end=2000))
```

</div>

</div>

</div>
