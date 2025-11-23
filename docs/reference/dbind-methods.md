<div id="main" class="col-md-9" role="main">

# Methods for binding objects of array classes along a given dimension

<div class="ref-description section level2">

These methods can bind two or more objects of array-based classes (e.g.
FLQuant), along the specified dimension.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
dbind(x, y, ...)

# S4 method for class 'FLArray,FLArray'
dbind(x, y, ..., dim = 1)

qbind(...)

ybind(...)

ubind(...)

sbind(...)

abind(...)

ibind(...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    First object to bind

-   y:

    Second object to bind

-   ...:

    Other objects to bind

-   dim:

    Dimension to bind on, *numeric* or *character*.

</div>

<div class="section level2">

## Value

An object of the same class as the inputs

</div>

<div class="section level2">

## Details

The objects to bind must contain the same dimmames in all dimensions
other than that used to bind, while dimnames in the selected one must
differ. See the examples below for correct and incorrect uses.

Object are bound in the order they are provided, with no attempt to sort
according to the dimnames of the chosen dimension.

The implementation is based around a single method (*dbind*), that
operates along the dimension position or name indicated by the *dim*
argument. A series of shortcut functions call the method for specific
dimensions, with names related to the dimensions name they operate on
(e.g. ybind for *year*).

</div>

<div class="section level2">

## See also

<div class="dont-index">

`FLQuant` `FLArray`

</div>

</div>

<div class="section level2">

## Author

Iago Mosqueira (EC JRC)

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
# By iter
x <- FLQuant(rnorm(80000), dim=c(4,20,1,1,1,1000))
y <- FLQuant(rnorm(80000), dim=c(4,20,1,1,1,1000))
  dimnames(y) <- list(iter=1001:2000)
ibind(x,y)
#> An object of class "FLQuant"
#> iters:  2000 
#> 
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1                2                3                4               
#>     1 -2.06e-02(1.025) -4.30e-02(1.036) -3.30e-02(0.992) -1.44e-02(0.999)
#>     2  1.92e-02(0.996)  1.03e-02(1.011) -2.67e-02(1.029)  1.93e-02(0.990)
#>     3  1.16e-02(1.012)  6.79e-03(1.017) -2.59e-02(1.021) -4.56e-03(1.023)
#>     4 -7.73e-03(1.017) -5.31e-02(1.004)  6.20e-03(0.963)  6.50e-02(0.993)
#>      year
#> quant 5                6                7                8               
#>     1  1.39e-02(1.006)  1.64e-02(0.983)  1.13e-03(0.931)  1.88e-02(0.956)
#>     2  1.84e-02(0.990)  3.43e-02(0.993)  8.22e-03(0.982)  1.03e-02(0.991)
#>     3 -2.76e-02(0.958)  2.82e-02(1.038) -2.90e-02(0.954) -3.02e-03(1.013)
#>     4  2.92e-02(1.028) -1.56e-02(1.007)  1.53e-02(0.984)  1.22e-02(0.995)
#>      year
#> quant 9                10               11               12              
#>     1  6.63e-06(1.015) -8.84e-03(0.961) -2.86e-03(0.988)  1.77e-02(1.008)
#>     2  3.49e-02(1.013) -3.72e-02(0.997) -1.44e-02(0.972)  1.52e-02(1.030)
#>     3 -9.93e-03(0.995) -1.32e-02(0.964) -1.18e-02(0.989) -2.25e-02(1.026)
#>     4 -5.71e-04(0.987)  2.18e-02(0.981)  2.96e-02(0.963)  4.38e-02(0.971)
#>      year
#> quant 13               14               15               16              
#>     1  7.39e-03(1.013)  1.02e-02(0.984) -3.58e-03(0.994) -3.45e-03(1.001)
#>     2 -2.08e-02(0.977) -1.58e-03(0.959) -3.35e-04(0.983)  4.65e-03(1.004)
#>     3  7.67e-03(1.047) -2.68e-03(0.965) -5.97e-02(0.962) -3.81e-03(0.962)
#>     4 -3.69e-02(0.950)  8.58e-03(0.959)  1.81e-03(1.009) -1.95e-02(0.991)
#>      year
#> quant 17               18               19               20              
#>     1  5.33e-03(1.061) -1.61e-02(1.000) -4.29e-02(1.006) -5.65e-04(1.078)
#>     2 -2.23e-02(1.022)  1.82e-02(1.013)  1.53e-02(1.056) -4.50e-02(0.985)
#>     3 -2.37e-02(0.973) -3.81e-03(0.975) -7.78e-03(0.987) -5.80e-02(1.008)
#>     4  3.89e-03(0.968) -4.08e-02(0.940)  1.68e-02(1.007)  2.49e-02(1.011)
#> 
#> units:  NA 

# By quant (age)
x <- FLQuant(1, dimnames=list(age=1:3, year=1:10))
y <- FLQuant(2, dimnames=list(age=4:12, year=1:10))
qbind(x, y)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  1 2 3 4 5 6 7 8 9 10
#>   1  1 1 1 1 1 1 1 1 1 1 
#>   2  1 1 1 1 1 1 1 1 1 1 
#>   3  1 1 1 1 1 1 1 1 1 1 
#>   4  2 2 2 2 2 2 2 2 2 2 
#>   5  2 2 2 2 2 2 2 2 2 2 
#>   6  2 2 2 2 2 2 2 2 2 2 
#>   7  2 2 2 2 2 2 2 2 2 2 
#>   8  2 2 2 2 2 2 2 2 2 2 
#>   9  2 2 2 2 2 2 2 2 2 2 
#>   10 2 2 2 2 2 2 2 2 2 2 
#>   11 2 2 2 2 2 2 2 2 2 2 
#>   12 2 2 2 2 2 2 2 2 2 2 
#> 
#> units:  NA 

# By year
x <- FLQuant(1, dimnames=list(age=1:3, year=1:10))
y <- FLQuant(2, dimnames=list(age=1:3, year=11:20))
z <- FLQuant(3, dimnames=list(age=1:3, year=21:30))
ybind(x, y, z)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#>   1 1 1 1 1 1 1 1 1 1 1  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3 
#>   2 1 1 1 1 1 1 1 1 1 1  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3 
#>   3 1 1 1 1 1 1 1 1 1 1  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3 
#>    year
#> age 29 30
#>   1 3  3 
#>   2 3  3 
#>   3 3  3 
#> 
#> units:  NA 

# By season
x <- FLQuant(1, dimnames=list(year=1:10, season=1:2))
y <- FLQuant(2, dimnames=list(year=1:10, season=3:4))
sbind(x, y)
#> An object of class "FLQuant"
#> , , unit = unique, season = 1, area = unique
#> 
#>      year
#> quant 1 2 3 4 5 6 7 8 9 10
#>   all 1 1 1 1 1 1 1 1 1 1 
#> 
#> , , unit = unique, season = 2, area = unique
#> 
#>      year
#> quant 1 2 3 4 5 6 7 8 9 10
#>   all 1 1 1 1 1 1 1 1 1 1 
#> 
#> , , unit = unique, season = 3, area = unique
#> 
#>      year
#> quant 1 2 3 4 5 6 7 8 9 10
#>   all 2 2 2 2 2 2 2 2 2 2 
#> 
#> , , unit = unique, season = 4, area = unique
#> 
#>      year
#> quant 1 2 3 4 5 6 7 8 9 10
#>   all 2 2 2 2 2 2 2 2 2 2 
#> 
#> units:  NA 
```

</div>

</div>

</div>
