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
#> <bytecode: 0x5577d0c60b78>
#> <environment: 0x557819fc6b28>
#> 
#> 
#> $catch.wt
#> ~catch.wt > 0
#> <environment: 0x557819fc6b28>
#> 
#> $landings.wt
#> ~landings.wt > 0
#> <environment: 0x557819fc6b28>
#> 
#> $discards.wt
#> ~discards.wt > 0
#> <environment: 0x557819fc6b28>
#> 
#> $stock.wt
#> ~stock.wt > 0
#> <environment: 0x557819fc6b28>
#> 
#> $mat
#> ~mat <= 1 & mat >= 0
#> <environment: 0x557819fc6b28>
#> 
#> $harvest.spwn
#> ~harvest.spwn <= 1 & harvest.spwn >= 0
#> <environment: 0x557819fc6b28>
#> 
#> $m.spwn
#> ~m.spwn <= 1 & m.spwn >= 0
#> <environment: 0x557819fc6b28>
#> 
#> $harvest
#> ~harvest >= 0
#> <environment: 0x557819fc6b28>
#> 
#> $cohorts
#> $cohorts$rule
#> ~ccohorts(stock.n)
#> <environment: 0x557819fc6b28>
#> 
#> $cohorts$ccohorts
#> function (x) 
#> {
#>     if (dim(x)[2] < dim(x)[1]) 
#>         return(NA)
#>     x <- FLCohort(x)[-dim(x)[1], seq(dim(x)[1], dim(x)[2] - dim(x)[1])]
#>     return((x[-1, ]/x[-dim(x)[1], ]) < 1)
#> }
#> <bytecode: 0x5577d0c60328>
#> <environment: 0x557819fc6b28>
#> 
#> 
#> $uoms
#> $uoms$rule
#> [1] "uoms"
#> 
#> $uoms$uoms
#> function (x) 
#> uomUnits(unlist(units(x)))
#> <bytecode: 0x5577d0c62c90>
#> <environment: 0x557819fc6b28>
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
#> <bytecode: 0x5577d0c60b78>
#> <environment: 0x55781a01a8c0>
#> 
#> 
```
