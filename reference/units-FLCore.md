# Method units

`units` attribute for FLQuant and FLArray-derived objects

## Usage

``` r
# S4 method for class 'FLArray'
units(x)

# S4 method for class 'FLArray,character'
units(x) <- value

setunits(x, value)

# S4 method for class 'FLPar'
units(x)

# S4 method for class 'FLPar,character'
units(x) <- value

# S4 method for class 'FLComp'
units(x)

# S4 method for class 'FLComp,list'
units(x) <- value

# S4 method for class 'FLComp,character'
units(x) <- value

# S4 method for class 'FLComp,function'
units(x) <- value
```

## Details

Objects of `FLArray`-based classes (e.g. [`FLQuant`](FLQuant.md))
contain a `units` attribute of class `character`. This should be used to
store the corresponding units of measurement. This attribute can be
directly accessed and modified using the `units` and `units<-` methods.

For complex objects, `units` will return a named list containing the
attributes of all `FLQuant` slots. `units` of a complex object can be
modified for all slots or a subset of them, by passing a named list with
the new values. See examples below.

The complete set of *units* for a complex object can be obtained as a
named *list*.

Assignment of *units* to the *FLQuant* slots of a complex object can be
carried out passing a named *list* or *character* vector containing the
units for the slots to be modified.

## Generic function

units(x)

units\<-(x,value)

## See also

[FLQuant](FLQuant.md), [FLPar](FLPar.md), [FLCohort](FLCohort.md)

## Author

The FLR Team

## Examples

``` r
flq <- FLQuant(rnorm(100), dim=c(5,20), units='kg')
units(flq)
#> [1] "kg"
units(flq) <- 't'
summary(flq)
#> An object of class "FLQuant" with:
#> 
#> dim:    quant year unit season area iter 
#>         5     20   1    1      1    1 
#> units:  t 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    %NAs 
#> -1.9623 -0.7002 -0.1438 -0.0017  0.6841  2.3884  0.0000 

# units for a complex object
  data(ple4)
  units(ple4)
#> $catch
#> [1] "t"
#> 
#> $catch.n
#> [1] "1000"
#> 
#> $catch.wt
#> [1] "kg"
#> 
#> $discards
#> [1] "t"
#> 
#> $discards.n
#> [1] "1000"
#> 
#> $discards.wt
#> [1] "kg"
#> 
#> $landings
#> [1] "t"
#> 
#> $landings.n
#> [1] "1000"
#> 
#> $landings.wt
#> [1] "kg"
#> 
#> $stock
#> [1] "t"
#> 
#> $stock.n
#> [1] "1000"
#> 
#> $stock.wt
#> [1] "kg"
#> 
#> $m
#> [1] "m"
#> 
#> $mat
#> [1] ""
#> 
#> $harvest
#> [1] "f"
#> 
#> $harvest.spwn
#> [1] ""
#> 
#> $m.spwn
#> [1] ""
#> 
  units(ple4) <- list(harvest='hr')

data(ple4)
units(ple4) <- list(harvest="hr")
units(ple4) <- c(harvest="hr")
```
