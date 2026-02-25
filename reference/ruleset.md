# Set of verify rules for an FLR class

Returns a set of standard rules to be used by the verify method for an
object of a given class.

## Usage

``` r
ruleset(object, ...)

# S4 method for class 'FLStock'
ruleset(object, ...)
```

## Arguments

- object:

  An object of any FLR class for which the method has been defined.

- ...:

  Names of positions in the standard list to subset.

## Value

A named list containing the rules defined for for the class object
belongs to.

## Details

A standard minimal set of rules to check FLStock objects against using
the verify method. The included rules are (with names in italics) check
that:

- there are no NAs in any slot, *anyna*.

- *catch.wt*, *landings.wt*, *discards.wt* and *stock.wt* \> 0.

- *mat*, *m.spwn* and *harvest.spwn* values are between 0 and 1.

- *harvest* \>= 0.

- *cohorts* in the stock.n slot contain decreasing numbers, except for
  the plusgroup age.

## Author

The FLR Team

## Examples

``` r
data(ple4)
ruleset(ple4)
#> $anyna
#> $anyna$rule
#> [1] "!anyna"
#> 
#> $anyna$anyna
#> function (x) 
#> unlist(qapply(x, function(y) sum(is.na(y), na.rm = TRUE) > 0))
#> <bytecode: 0x556063e25260>
#> <environment: 0x5560aa7133c8>
#> 
#> 
#> $catch.wt
#> ~catch.wt > 0
#> <environment: 0x5560aa7133c8>
#> 
#> $landings.wt
#> ~landings.wt > 0
#> <environment: 0x5560aa7133c8>
#> 
#> $discards.wt
#> ~discards.wt > 0
#> <environment: 0x5560aa7133c8>
#> 
#> $stock.wt
#> ~stock.wt > 0
#> <environment: 0x5560aa7133c8>
#> 
#> $mat
#> ~mat <= 1 & mat >= 0
#> <environment: 0x5560aa7133c8>
#> 
#> $harvest.spwn
#> ~harvest.spwn <= 1 & harvest.spwn >= 0
#> <environment: 0x5560aa7133c8>
#> 
#> $m.spwn
#> ~m.spwn <= 1 & m.spwn >= 0
#> <environment: 0x5560aa7133c8>
#> 
#> $harvest
#> ~harvest >= 0
#> <environment: 0x5560aa7133c8>
#> 
#> $cohorts
#> $cohorts$rule
#> ~ccohorts(stock.n)
#> <environment: 0x5560aa7133c8>
#> 
#> $cohorts$ccohorts
#> function (x) 
#> {
#>     if (dim(x)[2] < dim(x)[1]) 
#>         return(NA)
#>     x <- FLCohort(x)[-dim(x)[1], seq(dim(x)[1], dim(x)[2] - dim(x)[1])]
#>     return((x[-1, ]/x[-dim(x)[1], ]) < 1)
#> }
#> <bytecode: 0x556063e24a10>
#> <environment: 0x5560aa7133c8>
#> 
#> 
#> $uoms
#> $uoms$rule
#> [1] "uoms"
#> 
#> $uoms$uoms
#> function (x) 
#> uomUnits(unlist(units(x)))
#> <bytecode: 0x556063e27378>
#> <environment: 0x5560aa7133c8>
#> 
#> 
# Extract single rule by name
ruleset(ple4, 'anyna')
#> $anyna
#> $anyna$rule
#> [1] "!anyna"
#> 
#> $anyna$anyna
#> function (x) 
#> unlist(qapply(x, function(y) sum(is.na(y), na.rm = TRUE) > 0))
#> <bytecode: 0x556063e25260>
#> <environment: 0x5560aa78ec78>
#> 
#> 
```
