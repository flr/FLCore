# Methods for binding objects of array classes along a given dimension

These methods can bind two or more objects of array-based classes (e.g.
FLQuant), along the specified dimension.

## Usage

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

## Arguments

- x:

  First object to bind

- y:

  Second object to bind

- ...:

  Other objects to bind

- dim:

  Dimension to bind on, *numeric* or *character*.

## Value

An object of the same class as the inputs

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

## See also

[`FLQuant`](FLQuant.md) [`FLArray`](FLArray.md)

## Author

Iago Mosqueira (EC JRC)

## Examples

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
#>     1 -0.000821(1.077) -0.029317(1.014) -0.044677(1.028) -0.030460(0.965)
#>     2 -0.042900(0.990)  0.008226(0.996)  0.017026(1.017)  0.001458(1.029)
#>     3 -0.057355(0.972)  0.023531(1.019)  0.005803(1.000) -0.043455(1.029)
#>     4  0.041929(1.018)  0.007855(1.011) -0.035552(1.012)  0.003668(0.959)
#>      year
#> quant 5                6                7                8               
#>     1 -0.004342(1.009)  0.000677(1.017)  0.001724(0.964)  0.012196(0.938)
#>     2  0.025093(0.987)  0.016073(1.013)  0.053227(1.000) -0.017459(0.970)
#>     3 -0.019476(1.039) -0.031273(0.974)  0.014993(1.030) -0.027964(0.960)
#>     4  0.056515(1.016)  0.029381(1.017)  0.017147(0.987)  0.020228(0.997)
#>      year
#> quant 9                10               11               12              
#>     1  0.015827(0.974)  0.003219(1.006) -0.008842(0.967)  0.004661(0.983)
#>     2  0.004533(0.993)  0.025470(1.012) -0.070213(0.993) -0.012687(0.954)
#>     3  0.002288(1.023) -0.009341(1.001) -0.022673(0.959) -0.013098(0.998)
#>     4  0.009959(0.996)  0.017941(0.987)  0.020033(0.997)  0.049260(0.965)
#>      year
#> quant 13               14               15               16              
#>     1  0.028690(1.017)  0.032140(0.982)  0.011552(0.986) -0.006916(0.988)
#>     2  0.034839(1.023)  0.012169(0.960)  0.015299(0.975)  0.024786(1.005)
#>     3 -0.022550(1.018) -0.004815(1.059)  0.002305(0.983) -0.041625(0.978)
#>     4  0.026449(0.958) -0.056855(0.951)  0.006350(0.985)  0.001812(1.007)
#>      year
#> quant 17               18               19               20              
#>     1  0.013746(1.007) -0.026465(1.073) -0.009140(0.990) -0.042905(1.022)
#>     2  0.020233(1.030) -0.022942(1.005)  0.043681(1.023) -0.004448(1.074)
#>     3 -0.038172(0.969) -0.010554(0.992)  0.008902(0.963) -0.001071(0.985)
#>     4 -0.010131(0.990)  0.012789(0.978) -0.027446(0.933)  0.041203(1.010)
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
