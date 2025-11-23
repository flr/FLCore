<div id="main" class="col-md-9" role="main">

# Class FLlst

<div class="ref-description section level2">

`FLlst` is a class that extends `list` but implements a set of features
that give a little more structure to list objects. First the elements of
`FLlst` must all be of the same class. Second it implements a lock
mechanism that, when turned on, does not allow the user to increase or
decrease the object length.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
FLlst(object, ...)
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

[\[](https://rdrr.io/r/base/Extract.html), \[\<-, \[\[\<-, $\<-,
[coerce](https://rdrr.io/r/methods/setAs.html),
[lapply](https://rdrr.io/r/base/lapply.html),
[window](https://rdrr.io/r/stats/window.html),
[list](https://rdrr.io/r/base/list.html)

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
fll01 <- new("FLlst", list(a=1:10, b=10:20))
fll02 <- new("FLlst", list(1:10, 10:20), names=c("a","b"))
fll03 <- FLlst(a=1:10, b=10:20)
fll04 <- FLlst(list(a=1:10, b=10:20))
fll05 <- FLlst(c(1:10), c(10:20))
names(fll05) <- names(fll01)
names(fll01)
#> [1] "a" "b"
```

</div>

</div>

</div>
