# Extract

Extract or replace parts of an FLR Object

## Usage

``` r
# S4 method for class 'FLArray,ANY,ANY,ANY'
x[i, j, k, l, m, n, ..., drop = FALSE]

# S4 method for class 'FLArray,array,missing,missing'
x[i]

# S4 method for class 'FLArray,ANY,ANY,ANY'
x[i, j, k, l, m, n, ...] <- value

# S4 method for class 'FLArray,ANY,ANY,FLArray'
x[i, j, k, l, m, n, ...] <- value

# S4 method for class 'FLQuant'
x$name

# S4 method for class 'FLQuantDistr,ANY,ANY,ANY'
x[i, j, k, l, m, n, drop = FALSE]

# S4 method for class 'FLQuantDistr,array,missing,missing'
x[i]

# S4 method for class 'FLPar,ANY,ANY,ANY'
x[i, j, k, l, m, n, ..., drop = FALSE]

# S4 method for class 'FLPar,array,missing,missing'
x[i]

# S4 method for class 'FLPar,ANY,ANY,ANY'
x[i, j, k, l, m, n, ...] <- value

# S4 method for class 'FLPar'
x$name

# S4 method for class 'FLPar'
x$name <- value

# S4 method for class 'FLComp,ANY,ANY,ANY'
x[i, j, k, l, m, n, ..., drop = FALSE]

# S4 method for class 'FLComp,ANY,ANY,ANY'
x[i, j, k, l, m, n, ...] <- value

# S4 method for class 'FLStock,ANY,ANY,ANY'
x[i, j, k, l, m, n, ..., drop = FALSE]

# S4 method for class 'FLStock,ANY,ANY,FLStock'
x[i, j, k, l, m, n, ...] <- value

# S4 method for class 'FLI,ANY,ANY,ANY'
x[i, j, k, l, m, n, ..., drop = FALSE]

# S4 method for class 'predictModel,ANY,missing,ANY'
x[i, k, l, m, n, ..., drop = FALSE]

# S4 method for class 'FLlst,ANY,missing'
x[[i, j]] <- value

# S4 method for class 'FLlst'
x$name <- value

# S4 method for class 'FLlst,ANY,missing,ANY'
x[i, j] <- value

# S4 method for class 'FLlst,ANY,missing,ANY'
x[i, drop]
```

## Arguments

- x:

  object from which to extract or replace element(s)

- i, j, k, l, m, n:

  indices specifying elements to extract or replace on any of the six
  dimensions.

- ...:

  indices specifying elements to extract or replace by dimension name.

- drop:

  If 'TRUE' the result is coerced to the lowest possible dimension, and
  so might change class (e.g. drop='TRUE' on an `FLQuant` might return
  an `array` of less dimensions, a `matrix` or a `vector`.

- value:

  An object of the same class, or simpler if `drop=TRUE`, than 'x'.

- name:

  See [Extract](https://rdrr.io/r/base/Extract.html) for further
  details.

## Details

Operators acting on FLQuant, FLCohort, FLPar, FLComp, and derived
classes to extract or replace sections of an object.

Please note the differences between referencing sections of an object by
position using values of class `numeric`, or by using dimnames of class
`character`. See examples below.

All classes that are derived from `FLComp` (for example, `FLStock` and
`FLBiol`) can be subset along the six dimensions of their `FLQuant`
slots.

Classes that are derived from `FLlst` (for example, `FLStocks` and
`FLBiols`) can be subset in a similar way to ordinary list objects.

'\$' for the `FLPar` and `FLQuant` classes operate only along the first
dimension ('params' or 'quant'), and are provided to be used specially
in formulas.

## Generic function

- :

  x,i,j,drop

- :

  \[\<-(x,i,j,value)

- :

  \[\[\<-(x,i,j,value)

- :

  \\\<-(x,name,value)

## See also

[Extract](https://rdrr.io/r/base/Extract.html)

## Author

The FLR Team

## Examples

``` r
flq <- FLQuant(rnorm(200), dimnames=list(age=0:4, year=1991:2000,
  season=1:4))

# Extracting by position...
  flq[1,]
#> An object of class "FLQuant"
#> , , unit = unique, season = 1, area = unique
#> 
#>    year
#> age 1991    1992    1993    1994    1995    1996    1997    1998    1999   
#>   0  0.4242  1.6729 -0.3872 -0.6905 -0.2089  2.1269  2.0394  0.0223 -1.1462
#>    year
#> age 2000   
#>   0  0.4543
#> 
#> , , unit = unique, season = 2, area = unique
#> 
#>    year
#> age 1991    1992    1993    1994    1995    1996    1997    1998    1999   
#>   0 -0.1627  1.3218  0.4291  0.6777 -1.9001 -0.7883 -0.1029  1.0359 -1.0417
#>    year
#> age 2000   
#>   0  1.9931
#> 
#> , , unit = unique, season = 3, area = unique
#> 
#>    year
#> age 1991    1992    1993    1994    1995    1996    1997    1998    1999   
#>   0  1.1238  0.1330 -0.4294  0.0655 -1.1534  0.0751 -0.3389  1.2334  0.2374
#>    year
#> age 2000   
#>   0 -0.6395
#> 
#> , , unit = unique, season = 4, area = unique
#> 
#>    year
#> age 1991    1992    1993    1994    1995    1996    1997    1998    1999   
#>   0 -0.9028  1.3829 -0.8302 -0.8286  0.1497 -2.1022  0.0609  1.8777  1.0120
#>    year
#> age 2000   
#>   0  1.1298
#> 
#> units:  NA 
  flq[,1:5]
#> An object of class "FLQuant"
#> , , unit = unique, season = 1, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995    
#>   0  0.42419  1.67288 -0.38721 -0.69054 -0.20888
#>   1  1.06310 -0.35436 -0.78543 -0.55854 -1.39941
#>   2  1.04871  0.94635 -1.05674 -0.53666  0.25854
#>   3 -0.03810  1.31683 -0.79554  0.22713 -0.44180
#>   4  0.48615 -0.29664 -1.75628  0.97845  0.56860
#> 
#> , , unit = unique, season = 2, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995    
#>   0 -0.16268  1.32178  0.42915  0.67768 -1.90006
#>   1 -0.82731 -1.11971  0.12210  0.03850  0.93578
#>   2  1.87651  0.51460 -1.13801 -0.35638 -0.30905
#>   3  0.76644 -1.50910 -0.55802  0.78284  0.26307
#>   4  0.97996  1.53274  1.05254  0.80441 -1.79059
#> 
#> , , unit = unique, season = 3, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995    
#>   0  1.12384  0.13299 -0.42938  0.06549 -1.15340
#>   1 -0.39700  0.37650  1.36046 -1.09851 -0.34064
#>   2 -0.82326  1.13871 -0.07086 -0.63318  0.78636
#>   3 -0.57888  1.24126 -0.27215 -2.06365 -1.27051
#>   4  1.76379  0.61209 -2.44668  2.64893  0.54214
#> 
#> , , unit = unique, season = 4, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995    
#>   0 -0.90281  1.38291 -0.83021 -0.82860  0.14968
#>   1  1.31763  0.00313 -0.50359  0.28977 -1.43332
#>   2  1.10019 -0.07789 -1.19364 -0.48005 -0.01030
#>   3  1.20377  0.44143 -0.75172 -0.60483 -0.21224
#>   4 -1.43127  0.12892  1.45584  1.46011 -0.90634
#> 
#> units:  NA 
  flq[1:2,,,c(1,3)]
#> An object of class "FLQuant"
#> , , unit = unique, season = 1, area = unique
#> 
#>    year
#> age 1991    1992    1993    1994    1995    1996    1997    1998    1999   
#>   0  0.4242  1.6729 -0.3872 -0.6905 -0.2089  2.1269  2.0394  0.0223 -1.1462
#>   1  1.0631 -0.3544 -0.7854 -0.5585 -1.3994  0.4249  0.4495  0.6036  0.8462
#>    year
#> age 2000   
#>   0  0.4543
#>   1 -0.8552
#> 
#> , , unit = unique, season = 3, area = unique
#> 
#>    year
#> age 1991    1992    1993    1994    1995    1996    1997    1998    1999   
#>   0  1.1238  0.1330 -0.4294  0.0655 -1.1534  0.0751 -0.3389  1.2334  0.2374
#>   1 -0.3970  0.3765  1.3605 -1.0985 -0.3406  0.5585 -0.0756  0.3404 -1.3128
#>    year
#> age 2000   
#>   0 -0.6395
#>   1 -0.8452
#> 
#> units:  NA 

# ...by dimnames
  flq['0',]
#> An object of class "FLQuant"
#> , , unit = unique, season = 1, area = unique
#> 
#>    year
#> age 1991    1992    1993    1994    1995    1996    1997    1998    1999   
#>   0  0.4242  1.6729 -0.3872 -0.6905 -0.2089  2.1269  2.0394  0.0223 -1.1462
#>    year
#> age 2000   
#>   0  0.4543
#> 
#> , , unit = unique, season = 2, area = unique
#> 
#>    year
#> age 1991    1992    1993    1994    1995    1996    1997    1998    1999   
#>   0 -0.1627  1.3218  0.4291  0.6777 -1.9001 -0.7883 -0.1029  1.0359 -1.0417
#>    year
#> age 2000   
#>   0  1.9931
#> 
#> , , unit = unique, season = 3, area = unique
#> 
#>    year
#> age 1991    1992    1993    1994    1995    1996    1997    1998    1999   
#>   0  1.1238  0.1330 -0.4294  0.0655 -1.1534  0.0751 -0.3389  1.2334  0.2374
#>    year
#> age 2000   
#>   0 -0.6395
#> 
#> , , unit = unique, season = 4, area = unique
#> 
#>    year
#> age 1991    1992    1993    1994    1995    1996    1997    1998    1999   
#>   0 -0.9028  1.3829 -0.8302 -0.8286  0.1497 -2.1022  0.0609  1.8777  1.0120
#>    year
#> age 2000   
#>   0  1.1298
#> 
#> units:  NA 
  flq[,'1991']
#> An object of class "FLQuant"
#> , , unit = unique, season = 1, area = unique
#> 
#>    year
#> age 1991   
#>   0  0.4242
#>   1  1.0631
#>   2  1.0487
#>   3 -0.0381
#>   4  0.4861
#> 
#> , , unit = unique, season = 2, area = unique
#> 
#>    year
#> age 1991   
#>   0 -0.1627
#>   1 -0.8273
#>   2  1.8765
#>   3  0.7664
#>   4  0.9800
#> 
#> , , unit = unique, season = 3, area = unique
#> 
#>    year
#> age 1991   
#>   0  1.1238
#>   1 -0.3970
#>   2 -0.8233
#>   3 -0.5789
#>   4  1.7638
#> 
#> , , unit = unique, season = 4, area = unique
#> 
#>    year
#> age 1991   
#>   0 -0.9028
#>   1  1.3176
#>   2  1.1002
#>   3  1.2038
#>   4 -1.4313
#> 
#> units:  NA 
  flq[,as.character(1991:1995),,'1']
#> An object of class "FLQuant"
#> , , unit = unique, season = 1, area = unique
#> 
#>    year
#> age 1991    1992    1993    1994    1995   
#>   0  0.4242  1.6729 -0.3872 -0.6905 -0.2089
#>   1  1.0631 -0.3544 -0.7854 -0.5585 -1.3994
#>   2  1.0487  0.9463 -1.0567 -0.5367  0.2585
#>   3 -0.0381  1.3168 -0.7955  0.2271 -0.4418
#>   4  0.4861 -0.2966 -1.7563  0.9785  0.5686
#> 
#> units:  NA 

# Dimensions of length one can be drop
  flq[1, drop=TRUE]
#> [1] 0.4241876

# Replacing part of the object
  flq['0',,,1]<-0
```
