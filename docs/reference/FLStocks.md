<div id="main" class="col-md-9" role="main">

# Class FLStocks

<div class="ref-description section level2">

`FLStocks` is a class that extends `list` through `FLlst` but implements
a set of features that give a little bit more structure to list objects.
The elements of `FLStocks` must all be of class `FLStock`. It implements
a lock mechanism that, when turned on, does not allow the user to
increase or decrease the object length.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
FLStocks(object, ...)

# S4 method for class 'FLStock'
FLStocks(object, ...)

# S4 method for class 'missing'
FLStocks(object, ...)

# S4 method for class 'list'
FLStocks(object, ...)
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

[plot](https://rdrr.io/r/graphics/plot.default.html), [FLlst](FLlst.md),
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
data(ple4)
fls <- FLStocks(sa=ple4, sb=window(ple4, end=1980))
summary(fls)
#> An object of class "FLStocks"
#> 
#> Elements: sa sb 
#> 
#> Name: PLE 
#>  Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#>  Range:   min    max pgroup  minyear maxyear minfbar maxfbar 
#>      1   10  10  1957    2017    2   6   
#>  Quant: age 
#>  dim: 10 61 1 1 1 
#> Name: PLE 
#>  Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#>  Range:   min    max pgroup  minyear maxyear minfbar maxfbar 
#>      1   10  10  1957    1980    2   6   
#>  Quant: age 
#>  dim: 10 24 1 1 1 
```

</div>

</div>

</div>
