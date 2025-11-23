<div id="main" class="col-md-9" role="main">

# Class FLPars

<div class="ref-description section level2">

A list of `FLPar` objects.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
FLPars(object, ...)

# S4 method for class 'ANY'
FLPars(object, ...)

# S4 method for class 'missing'
FLPars(object, ...)

# S4 method for class 'list'
FLPars(object)

# S4 method for class 'FLPars'
FLPars(object)
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

    Internal S4 data representation, of class `list`.

-   desc:

    As textual description of the object contents

-   lock:

    Can the object be extended/trimmed? `TRUE` or `FALSE`.

-   names:

    A character vector for the element names

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
