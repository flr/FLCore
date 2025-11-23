<div id="main" class="col-md-9" role="main">

# `FLSRS` is a class that extends `list` through `FLlst` but implements a set of features that give a little bit more structure to list objects. The elements of `FLSRs` must all be of class `FLSR`. It implements a lock mechanism that, when turned on, does not allow the user to increase or decrease the object length.

<div class="ref-description section level2">

`FLSRS` is a class that extends `list` through `FLlst` but implements a
set of features that give a little bit more structure to list objects.
The elements of `FLSRs` must all be of class `FLSR`. It implements a
lock mechanism that, when turned on, does not allow the user to increase
or decrease the object length.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
FLSRs(object, ...)

# S4 method for class 'FLSR'
FLSRs(object, ...)

# S4 method for class 'missing'
FLSRs(object, ...)

# S4 method for class 'list'
FLSRs(object, ...)
```

</div>

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

## See also

<div class="dont-index">

[FLlst](FLlst.md), [list](https://rdrr.io/r/base/list.html),
[FLSR](FLSR.md)

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
data(nsher)
bnsher <- nsher
model(bnsher) <- bevholt
bnsher <- fmle(bnsher)
#>   Nelder-Mead direct search function minimizer
#> function value for initial parameters = -10.336211
#>   Scaled convergence tolerance is 1.54022e-07
#> Stepsize computed as 501.110000
#> BUILD              3 44.842344 -11.603908
#> Warning: NaNs produced
#> HI-REDUCTION       5 31.685209 -11.603908
#> Warning: NaNs produced
#> HI-REDUCTION       7 17.913114 -11.603908
#> Warning: NaNs produced
#> HI-REDUCTION       9 5.415279 -11.603908
#> Warning: NaNs produced
#> HI-REDUCTION      11 -3.412974 -11.603908
#> HI-REDUCTION      13 -8.018030 -11.603908
#> LO-REDUCTION      15 -10.336211 -11.603908
#> LO-REDUCTION      17 -11.081040 -11.603908
#> EXTENSION         19 -11.295930 -12.061705
#> LO-REDUCTION      21 -11.603908 -12.061705
#> REFLECTION        23 -11.813826 -12.087620
#> REFLECTION        25 -12.061705 -12.199591
#> LO-REDUCTION      27 -12.087620 -12.199591
#> LO-REDUCTION      29 -12.158184 -12.199591
#> LO-REDUCTION      31 -12.191726 -12.199591
#> HI-REDUCTION      33 -12.192269 -12.199591
#> HI-REDUCTION      35 -12.197784 -12.199591
#> LO-REDUCTION      37 -12.198015 -12.199591
#> HI-REDUCTION      39 -12.199555 -12.199776
#> REFLECTION        41 -12.199591 -12.200058
#> HI-REDUCTION      43 -12.199776 -12.200092
#> HI-REDUCTION      45 -12.200058 -12.200142
#> HI-REDUCTION      47 -12.200092 -12.200155
#> HI-REDUCTION      49 -12.200142 -12.200160
#> HI-REDUCTION      51 -12.200155 -12.200177
#> HI-REDUCTION      53 -12.200160 -12.200177
#> LO-REDUCTION      55 -12.200171 -12.200179
#> HI-REDUCTION      57 -12.200177 -12.200179
#> HI-REDUCTION      59 -12.200178 -12.200179
#> HI-REDUCTION      61 -12.200179 -12.200179
#> HI-REDUCTION      63 -12.200179 -12.200179
#> HI-REDUCTION      65 -12.200179 -12.200179
#> Exiting from Nelder Mead minimizer
#>     67 function evaluations used
fls <- FLSRs(Ricker=nsher, BevHolt=bnsher)
summary(fls)
#> An object of class "FLSRs"
#> 
#> Elements: Ricker BevHolt 
#> 
#> Name:  
#>  Description:  
#>  Range:   min    minyear max maxyear 
#>      0   1960    0   2004    
#>  Quant: age 
#>  dim: 1 45 1 1 1 
#> Name:  
#>  Description:  
#>  Range:   min    minyear max maxyear 
#>      0   1960    0   2004    
#>  Quant: age 
#>  dim: 1 45 1 1 1 
```

</div>

</div>

</div>
