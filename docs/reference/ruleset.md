<div id="main" class="col-md-9" role="main">

# Set of verify rules for an FLR class

<div class="ref-description section level2">

Returns a set of standard rules to be used by the verify method for an
object of a given class.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
ruleset(object, ...)

# S4 method for class 'FLStock'
ruleset(object, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    An object of any FLR class for which the method has been defined.

-   ...:

    Names of positions in the standard list to subset.

</div>

<div class="section level2">

## Value

A named list containing the rules defined for for the class object
belongs to.

</div>

<div class="section level2">

## Details

A standard minimal set of rules to check FLStock objects against using
the verify method. The included rules are (with names in italics) check
that:

-   there are no NAs in any slot, *anyna*.

-   *catch.wt*, *landings.wt*, *discards.wt* and *stock.wt* \> 0.

-   *mat*, *m.spwn* and *harvest.spwn* values are between 0 and 1.

-   *harvest* \>= 0.

-   *cohorts* in the stock.n slot contain decreasing numbers, except for
    the plusgroup age.

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
ruleset(ple4)
#> $anyna
#> $anyna$rule
#> [1] "!anyna"
#> 
#> $anyna$anyna
#> function(x)
#>       unlist(qapply(x, function(y) sum(is.na(y), na.rm=TRUE) > 0))
#> <bytecode: 0x563fa8ba7548>
#> <environment: 0x563fda61a5a8>
#> 
#> 
#> $catch.wt
#> ~catch.wt > 0
#> <environment: 0x563fda61a5a8>
#> 
#> $landings.wt
#> ~landings.wt > 0
#> <environment: 0x563fda61a5a8>
#> 
#> $discards.wt
#> ~discards.wt > 0
#> <environment: 0x563fda61a5a8>
#> 
#> $stock.wt
#> ~stock.wt > 0
#> <environment: 0x563fda61a5a8>
#> 
#> $mat
#> ~mat <= 1 & mat >= 0
#> <environment: 0x563fda61a5a8>
#> 
#> $harvest.spwn
#> ~harvest.spwn <= 1 & harvest.spwn >= 0
#> <environment: 0x563fda61a5a8>
#> 
#> $m.spwn
#> ~m.spwn <= 1 & m.spwn >= 0
#> <environment: 0x563fda61a5a8>
#> 
#> $harvest
#> ~harvest >= 0
#> <environment: 0x563fda61a5a8>
#> 
#> $cohorts
#> $cohorts$rule
#> ~ccohorts(stock.n)
#> <environment: 0x563fda61a5a8>
#> 
#> $cohorts$ccohorts
#> function(x) {
#>         if(dim(x)[2] < dim(x)[1])
#>           return(NA)
#>         #   DROP plusgroup, SELECT full cohorts
#>         x <- FLCohort(x)[-dim(x)[1], seq(dim(x)[1], dim(x)[2] - dim(x)[1])]
#>         # CHECK cohort change in abundances
#>         return((x[-1, ] / x[-dim(x)[1],]) < 1)
#>       }
#> <bytecode: 0x563fa8bac1c0>
#> <environment: 0x563fda61a5a8>
#> 
#> 
#> $uoms
#> $uoms$rule
#> [1] "uoms"
#> 
#> $uoms$uoms
#> function(x)
#>       uomUnits(unlist(units(x)))
#> <bytecode: 0x563fa8bb4000>
#> <environment: 0x563fda61a5a8>
#> 
#> 
# Extract single rule by name
ruleset(ple4, 'anyna')
#> $anyna
#> $anyna$rule
#> [1] "!anyna"
#> 
#> $anyna$anyna
#> function(x)
#>       unlist(qapply(x, function(y) sum(is.na(y), na.rm=TRUE) > 0))
#> <bytecode: 0x563fa8ba7548>
#> <environment: 0x563fc6a27658>
#> 
#> 
```

</div>

</div>

</div>
