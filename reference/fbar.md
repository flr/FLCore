# Mean fishing mortality over a selected age range

Computes the arithmetic mean of the [`harvest()`](accessors.md) slot
over a contiguous range of ages.

## Usage

``` r
fbar(object, ...)

# S4 method for class 'FLStock'
fbar(object, min = range(object, "minfbar"), max = range(object, "maxfbar"))

# S4 method for class 'FLBiol'
fbar(object, minAge = dims(object)$min + 1, maxAge = dims(object)$max - 1, ...)
```

## Arguments

- object:

  An [FLStock](FLStock.md) object.

- min:

  Integer. Minimum age to include in the mean. Defaults to the value
  stored in `range(object, "minfbar")`; if that value is `NA`, the
  smallest age in the object (`range(object, "min")`) is used instead.

- max:

  Integer. Maximum age to include in the mean. Defaults to the value
  stored in `range(object, "maxfbar")`; if that value is `NA`, the
  largest age in the object (`range(object, "max")`) is used instead.

- minAge:

  Minimum age. Defaults to min + 1.

- maxAge:

  Maximum age. Defaults to max - 1.

## Value

An [FLQuant](FLQuant.md) with the `quant` dimension collapsed to one,
containing the arithmetic mean of [`harvest()`](accessors.md) over ages
`min` to `max` inclusive, as computed by
[`quantMeans()`](dimSummaries.md).

## Details

Overriding `min` and `max` directly allows a one-off calculation over a
different age range without modifying the object.

The method accepts both `"f"` and `"hr"` as valid units for the
[`harvest()`](accessors.md) slot and applies
[`quantMeans()`](dimSummaries.md) identically in both cases.

## Methods (by class)

- `fbar(FLStock)`: The `minfbar` and `maxfbar` entries of the `range`
  slot are specific to the [FLStock](FLStock.md) class, and define the
  reference age range for fishing-mortality calculations. These are set
  when the [FLStock](FLStock.md) object is created or read, and can be
  altered using .

- `fbar(FLBiol)`: For FLBiol objects, fbar is calculated from stock
  numbers (n) and natural mortality (m) by estimating fishing mortality
  from the cohort decline, i.e., F = log(nage/nage+1) - Mage

## See also

[`harvest()`](accessors.md), [`quantMeans()`](dimSummaries.md),
[`base::range()`](https://rdrr.io/r/base/range.html)

[FLStock](FLStock.md)

## Author

FLR Team

## Examples

``` r
data(ple4)

# Default fbar using minfbar/maxfbar from range slot
fbar(ple4)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957  1958  1959  1960  1961  1962  1963  1964  1965  1966  1967  1968 
#>   all 0.242 0.279 0.311 0.324 0.325 0.336 0.367 0.394 0.388 0.362 0.348 0.354
#>      year
#> age   1969  1970  1971  1972  1973  1974  1975  1976  1977  1978  1979  1980 
#>   all 0.364 0.363 0.367 0.399 0.452 0.480 0.461 0.437 0.449 0.488 0.526 0.542
#>      year
#> age   1981  1982  1983  1984  1985  1986  1987  1988  1989  1990  1991  1992 
#>   all 0.541 0.526 0.515 0.523 0.551 0.586 0.616 0.630 0.622 0.606 0.600 0.607
#>      year
#> age   1993  1994  1995  1996  1997  1998  1999  2000  2001  2002  2003  2004 
#>   all 0.610 0.598 0.598 0.642 0.712 0.724 0.650 0.582 0.572 0.594 0.571 0.477
#>      year
#> age   2005  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016 
#>   all 0.387 0.333 0.296 0.255 0.216 0.196 0.198 0.207 0.210 0.205 0.201 0.199
#>      year
#> age   2017 
#>   all 0.199
#> 
#> units:  f 

# Override age range for a one-off calculation
fbar(ple4, min = 3, max = 6)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957  1958  1959  1960  1961  1962  1963  1964  1965  1966  1967  1968 
#>   all 0.259 0.295 0.326 0.340 0.343 0.356 0.391 0.423 0.420 0.394 0.376 0.372
#>      year
#> age   1969  1970  1971  1972  1973  1974  1975  1976  1977  1978  1979  1980 
#>   all 0.373 0.372 0.381 0.420 0.478 0.506 0.478 0.446 0.455 0.497 0.538 0.558
#>      year
#> age   1981  1982  1983  1984  1985  1986  1987  1988  1989  1990  1991  1992 
#>   all 0.560 0.547 0.537 0.547 0.578 0.617 0.650 0.667 0.663 0.650 0.644 0.649
#>      year
#> age   1993  1994  1995  1996  1997  1998  1999  2000  2001  2002  2003  2004 
#>   all 0.651 0.645 0.652 0.700 0.772 0.787 0.717 0.648 0.625 0.625 0.582 0.481
#>      year
#> age   2005  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016 
#>   all 0.388 0.328 0.286 0.245 0.210 0.196 0.202 0.214 0.214 0.206 0.200 0.201
#>      year
#> age   2017 
#>   all 0.206
#> 
#> units:  f 

# Inspect the reference age range stored in the object
range(ple4, c("minfbar", "maxfbar"))
#> minfbar maxfbar 
#>       2       6 
```
