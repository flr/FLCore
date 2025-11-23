<div id="main" class="col-md-9" role="main">

# Names of slots of a given class

<div class="ref-description section level2">

This function returns the names, as a character vector, of the slots in
an S4 object that are of the class specified by the 'class' argument.
Comparison is done using is(), so class inheritance is matched.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
getSlotNamesClass(object, class)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    An S4 object to check slots from.

-   class:

    The name of the class to match, 'character'.

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
getSlotNamesClass(ple4, 'FLQuant')
#>  [1] "catch"        "catch.n"      "catch.wt"     "discards"     "discards.n"  
#>  [6] "discards.wt"  "landings"     "landings.n"   "landings.wt"  "stock"       
#> [11] "stock.n"      "stock.wt"     "m"            "mat"          "harvest"     
#> [16] "harvest.spwn" "m.spwn"      
```

</div>

</div>

</div>
