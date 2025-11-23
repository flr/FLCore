<div id="main" class="col-md-9" role="main">

# Class FLModelSims

<div class="ref-description section level2">

A list of `FLModelSim` objects.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

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

[FLlst](FLlst.md), [list](https://rdrr.io/r/base/list.html),
[vector](https://rdrr.io/r/base/vector.html)

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

</div>
