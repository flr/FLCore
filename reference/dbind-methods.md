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
#>     1  0.023369(1.003)  0.020831(1.031) -0.022509(1.006)  0.043341(1.023)
#>     2 -0.041625(0.979) -0.038172(0.968) -0.011214(0.992)  0.009425(0.963)
#>     3  0.001154(1.007) -0.007503(0.995)  0.011562(0.980) -0.027446(0.933)
#>     4  0.013746(1.007) -0.027552(1.073) -0.008830(0.989) -0.037313(1.028)
#>      year
#> quant 5                6                7                8               
#>     1 -0.001855(1.075) -0.042900(0.990)  0.009883(0.995)  0.015639(1.015)
#>     2 -0.005121(0.989) -0.055817(0.971)  0.023531(1.019)  0.005803(1.001)
#>     3  0.043326(1.010)  0.041424(1.018)  0.007387(1.010) -0.035552(1.012)
#>     4 -0.000821(1.077) -0.029317(1.014) -0.045816(1.027) -0.028998(0.964)
#>      year
#> quant 9                10               11               12              
#>     1  0.001458(1.029)  0.025093(0.987)  0.016073(1.013)  0.053227(1.001)
#>     2 -0.043455(1.027) -0.019476(1.040) -0.030831(0.975)  0.013923(1.030)
#>     3  0.002092(0.959)  0.054971(1.014)  0.029381(1.017)  0.017147(0.987)
#>     4 -0.004342(1.010) -0.000583(1.019)  0.001209(0.963)  0.011684(0.941)
#>      year
#> quant 13               14               15               16              
#>     1 -0.017459(0.970)  0.005440(0.992)  0.025470(1.012) -0.070213(0.993)
#>     2 -0.027964(0.960)  0.001476(1.023) -0.008969(1.000) -0.022673(0.959)
#>     3  0.020228(0.996)  0.009222(0.998)  0.017941(0.985)  0.019294(0.997)
#>     4  0.017173(0.975)  0.002024(1.006) -0.005726(0.965)  0.002866(0.983)
#>      year
#> quant 17               18               19               20              
#>     1 -0.011884(0.953)  0.034839(1.023)  0.012169(0.961)  0.013945(0.975)
#>     2 -0.013098(0.998) -0.022550(1.018) -0.004815(1.059)  0.002305(0.983)
#>     3  0.049762(0.964)  0.025800(0.958) -0.059218(0.948)  0.006350(0.985)
#>     4  0.028690(1.017)  0.029870(0.983)  0.011552(0.987) -0.007443(0.989)
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
