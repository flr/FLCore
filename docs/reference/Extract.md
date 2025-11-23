<div id="main" class="col-md-9" role="main">

# Extract

<div class="ref-description section level2">

Extract or replace parts of an FLR Object

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

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

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    object from which to extract or replace element(s)

-   i, j, k, l, m, n:

    indices specifying elements to extract or replace on any of the six
    dimensions.

-   ...:

    indices specifying elements to extract or replace by dimension name.

-   drop:

    If 'TRUE' the result is coerced to the lowest possible dimension,
    and so might change class (e.g. drop='TRUE' on an `FLQuant` might
    return an `array` of less dimensions, a `matrix` or a `vector`.

-   value:

    An object of the same class, or simpler if `drop=TRUE`, than 'x'.

-   name:

    See [Extract](https://rdrr.io/r/base/Extract.html) for further
    details.

</div>

<div class="section level2">

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

'$' for the `FLPar` and `FLQuant` classes operate only along the first
dimension ('params' or 'quant'), and are provided to be used specially
in formulas.

</div>

<div class="section level2">

## Generic function

-   :

    x,i,j,drop

-   :

    \[\<-(x,i,j,value)

-   :

    \[\[\<-(x,i,j,value)

-   :

    \\$\<-(x,name,value)

</div>

<div class="section level2">

## See also

<div class="dont-index">

[Extract](https://rdrr.io/r/base/Extract.html)

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
flq <- FLQuant(rnorm(200), dimnames=list(age=0:4, year=1991:2000,
  season=1:4))

# Extracting by position...
  flq[1,]
#> An object of class "FLQuant"
#> , , unit = unique, season = 1, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995     1996     1997     1998    
#>   0  0.48615 -0.29664 -1.75628  0.97845  0.56860  1.07284  0.10758  0.19215
#>    year
#> age 1999     2000    
#>   0 -0.94491  0.06730
#> 
#> , , unit = unique, season = 2, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995     1996     1997     1998    
#>   0  0.97996  1.53274  1.05254  0.80441 -1.79059  0.51767  0.76872  2.21177
#>    year
#> age 1999     2000    
#>   0  0.55419  1.14269
#> 
#> , , unit = unique, season = 3, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995     1996     1997     1998    
#>   0  1.76379  0.61209 -2.44668  2.64893  0.54214  0.94121 -0.99843 -1.52896
#>    year
#> age 1999     2000    
#>   0  0.07105 -1.68650
#> 
#> , , unit = unique, season = 4, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995     1996     1997     1998    
#>   0 -1.43127  0.12892  1.45584  1.46011 -0.90634  0.23996  0.00789 -0.30821
#>    year
#> age 1999     2000    
#>   0  0.36667 -0.38373
#> 
#> units:  NA 
  flq[,1:5]
#> An object of class "FLQuant"
#> , , unit = unique, season = 1, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995    
#>   0  0.48615 -0.29664 -1.75628  0.97845  0.56860
#>   1  1.67288 -0.38721 -0.69054 -0.20888  2.12685
#>   2 -0.35436 -0.78543 -0.55854 -1.39941  0.42486
#>   3  0.94635 -1.05674 -0.53666  0.25854 -1.68428
#>   4  1.31683 -0.79554  0.22713 -0.44180  0.24940
#> 
#> , , unit = unique, season = 2, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995    
#>   0  0.97996  1.53274  1.05254  0.80441 -1.79059
#>   1  1.32178  0.42915  0.67768 -1.90006 -0.78826
#>   2 -1.11971  0.12210  0.03850  0.93578 -1.13302
#>   3  0.51460 -1.13801 -0.35638 -0.30905  0.36365
#>   4 -1.50910 -0.55802  0.78284  0.26307 -0.28589
#> 
#> , , unit = unique, season = 3, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995    
#>   0  1.76379  0.61209 -2.44668  2.64893  0.54214
#>   1  0.13299 -0.42938  0.06549 -1.15340  0.07511
#>   2  0.37650  1.36046 -1.09851 -0.34064  0.55851
#>   3  1.13871 -0.07086 -0.63318  0.78636  0.41541
#>   4  1.24126 -0.27215 -2.06365 -1.27051 -1.45230
#> 
#> , , unit = unique, season = 4, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995    
#>   0 -1.43127  0.12892  1.45584  1.46011 -0.90634
#>   1  1.38291 -0.83021 -0.82860  0.14968 -2.10215
#>   2  0.00313 -0.50359  0.28977 -1.43332  1.89336
#>   3 -0.07789 -1.19364 -0.48005 -0.01030 -0.96813
#>   4  0.44143 -0.75172 -0.60483 -0.21224 -0.10260
#> 
#> units:  NA 
  flq[1:2,,,c(1,3)]
#> An object of class "FLQuant"
#> , , unit = unique, season = 1, area = unique
#> 
#>    year
#> age 1991    1992    1993    1994    1995    1996    1997    1998    1999   
#>   0  0.4861 -0.2966 -1.7563  0.9785  0.5686  1.0728  0.1076  0.1921 -0.9449
#>   1  1.6729 -0.3872 -0.6905 -0.2089  2.1269  2.0394  0.0223 -1.1462  0.4543
#>    year
#> age 2000   
#>   0  0.0673
#>   1 -0.1627
#> 
#> , , unit = unique, season = 3, area = unique
#> 
#>    year
#> age 1991    1992    1993    1994    1995    1996    1997    1998    1999   
#>   0  1.7638  0.6121 -2.4467  2.6489  0.5421  0.9412 -0.9984 -1.5290  0.0711
#>   1  0.1330 -0.4294  0.0655 -1.1534  0.0751 -0.3389  1.2334  0.2374 -0.6395
#>    year
#> age 2000   
#>   0 -1.6865
#>   1 -0.9028
#> 
#> units:  NA 

# ...by dimnames
  flq['0',]
#> An object of class "FLQuant"
#> , , unit = unique, season = 1, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995     1996     1997     1998    
#>   0  0.48615 -0.29664 -1.75628  0.97845  0.56860  1.07284  0.10758  0.19215
#>    year
#> age 1999     2000    
#>   0 -0.94491  0.06730
#> 
#> , , unit = unique, season = 2, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995     1996     1997     1998    
#>   0  0.97996  1.53274  1.05254  0.80441 -1.79059  0.51767  0.76872  2.21177
#>    year
#> age 1999     2000    
#>   0  0.55419  1.14269
#> 
#> , , unit = unique, season = 3, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995     1996     1997     1998    
#>   0  1.76379  0.61209 -2.44668  2.64893  0.54214  0.94121 -0.99843 -1.52896
#>    year
#> age 1999     2000    
#>   0  0.07105 -1.68650
#> 
#> , , unit = unique, season = 4, area = unique
#> 
#>    year
#> age 1991     1992     1993     1994     1995     1996     1997     1998    
#>   0 -1.43127  0.12892  1.45584  1.46011 -0.90634  0.23996  0.00789 -0.30821
#>    year
#> age 1999     2000    
#>   0  0.36667 -0.38373
#> 
#> units:  NA 
  flq[,'1991']
#> An object of class "FLQuant"
#> , , unit = unique, season = 1, area = unique
#> 
#>    year
#> age 1991    
#>   0  0.48615
#>   1  1.67288
#>   2 -0.35436
#>   3  0.94635
#>   4  1.31683
#> 
#> , , unit = unique, season = 2, area = unique
#> 
#>    year
#> age 1991    
#>   0  0.97996
#>   1  1.32178
#>   2 -1.11971
#>   3  0.51460
#>   4 -1.50910
#> 
#> , , unit = unique, season = 3, area = unique
#> 
#>    year
#> age 1991    
#>   0  1.76379
#>   1  0.13299
#>   2  0.37650
#>   3  1.13871
#>   4  1.24126
#> 
#> , , unit = unique, season = 4, area = unique
#> 
#>    year
#> age 1991    
#>   0 -1.43127
#>   1  1.38291
#>   2  0.00313
#>   3 -0.07789
#>   4  0.44143
#> 
#> units:  NA 
  flq[,as.character(1991:1995),,'1']
#> An object of class "FLQuant"
#> , , unit = unique, season = 1, area = unique
#> 
#>    year
#> age 1991   1992   1993   1994   1995  
#>   0  0.486 -0.297 -1.756  0.978  0.569
#>   1  1.673 -0.387 -0.691 -0.209  2.127
#>   2 -0.354 -0.785 -0.559 -1.399  0.425
#>   3  0.946 -1.057 -0.537  0.259 -1.684
#>   4  1.317 -0.796  0.227 -0.442  0.249
#> 
#> units:  NA 

# Dimensions of length one can be drop
  flq[1, drop=TRUE]
#> [1] 0.4861489

# Replacing part of the object
  flq['0',,,1]<-0
```

</div>

</div>

</div>
