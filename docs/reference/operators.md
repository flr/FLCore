<div id="main" class="col-md-9" role="main">

# FLQuant arithmetic operators that extend objects

<div class="ref-description section level2">

Arithmetic operations between two [FLQuant](FLQuant.md) objects using
the standars operators (`+`, `-`, `*`, `/`, `^`, see Arith) need all
dimensions in both objects to match. This requirement is relaxed by
using the percent version of those five operators: `%+%`, `%-%`, `%*%`,
`%/%` and `%^%`.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
e1 %+% e2

x %-% y

x %^% y

# S4 method for class 'FLQuant,FLQuant'
x %*% y

# S4 method for class 'FLQuant,FLQuant'
e1%/%e2

# S4 method for class 'FLQuant,FLQuant'
e1 %+% e2

# S4 method for class 'FLQuant,FLQuant'
x %-% y

# S4 method for class 'FLQuant,FLQuant'
x %^% y

# S4 method for class 'FLPar,FLQuant'
x %*% y

# S4 method for class 'FLPar,FLQuant'
e1%/%e2

# S4 method for class 'FLPar,FLQuant'
e1 %+% e2

# S4 method for class 'FLPar,FLQuant'
x %-% y

# S4 method for class 'FLPar,FLQuant'
x %^% y

# S4 method for class 'FLQuant,FLPar'
x %*% y

# S4 method for class 'FLQuant,FLPar'
e1%/%e2

# S4 method for class 'FLQuant,FLPar'
e1 %+% e2

# S4 method for class 'FLQuant,FLPar'
x %-% y

# S4 method for class 'FLQuant,FLPar'
x %^% y

# S4 method for class 'FLPar,FLPar'
x %*% y

# S4 method for class 'FLPar,FLPar'
e1 %+% e2

# S4 method for class 'FLPar,FLPar'
x %-% y

# S4 method for class 'FLPar,FLPar'
e1%/%e2

# S4 method for class 'FLPar,FLPar'
x %^% y

# S4 method for class 'FLQuants,FLPar'
e1/e2

# S4 method for class 'FLQuants,FLPar'
e1 * e2

# S4 method for class 'FLQuants,FLPars'
e1/e2

# S4 method for class 'FLQuants,FLPars'
e1 * e2

# S4 method for class 'FLQuants,FLQuants'
e1/e2

# S4 method for class 'FLQuants,FLQuants'
e1 * e2

# S4 method for class 'FLQuants,FLQuants'
e1 + e2

# S4 method for class 'FLQuants,FLQuants'
e1 - e2
```

</div>

</div>

<div class="section level2">

## Details

If any of the objects is of length one in a dimensions where the other
is longer, the dimensions will be extended and the element-by-element
operation then conducted. Dimensions and dimnames of the output will be
those of the larger object. See the examples to observe their behaviour.

Please note that this behaviour is already present on the Arith methods
for [FLArray](FLArray.md)-derived classes but only on the 6th, `iter`,
dimension.

The original use of the `%*%` operator, as vector product, is not
available for [FLQuant](FLQuant.md) objects, but can be applied to the
[array](https://rdrr.io/r/base/array.html) inside them, as in the
example below.

Methods for operations between an [FLQuant](FLQuant.md) and an
[FLPar](FLPar.md) object will match dimensions by names of dimnames,
regardless of position.

</div>

<div class="section level2">

## Generic function

x %+% y, x %-% y, x %\*% y, e1 %/% e2, x %^% y

</div>

<div class="section level2">

## See also

<div class="dont-index">

`FLQuant`, `matmult`

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
a <- FLQuant(2, dim=c(3,3,2))
b <- FLQuant(3, dim=c(3,3,1))

# This should fail
if (FALSE)  a * b  # \dontrun{}

a %*% b
#> An object of class "FLQuant"
#> , , unit = 1, season = all, area = unique
#> 
#>      year
#> quant 1 2 3
#>     1 6 6 6
#>     2 6 6 6
#>     3 6 6 6
#> 
#> , , unit = 2, season = all, area = unique
#> 
#>      year
#> quant 1 2 3
#>     1 6 6 6
#>     2 6 6 6
#>     3 6 6 6
#> 
#> units:  NA 
a %+% b
#> An object of class "FLQuant"
#> , , unit = 1, season = all, area = unique
#> 
#>      year
#> quant 1 2 3
#>     1 5 5 5
#>     2 5 5 5
#>     3 5 5 5
#> 
#> , , unit = 2, season = all, area = unique
#> 
#>      year
#> quant 1 2 3
#>     1 5 5 5
#>     2 5 5 5
#>     3 5 5 5
#> 
#> units:  NA 
# To use base's %*% vector product, apply it to a matrix from @.Data
b@.Data[,,,,,] %*% 1:3
#>      
#> quant [,1]
#>     1   18
#>     2   18
#>     3   18
# or
b[,,drop=TRUE] %*% 1:3
#>      
#> quant [,1]
#>     1   18
#>     2   18
#>     3   18

# FLPar vs. FLQuant works by dimnames' names
flp <- FLPar(2, dimnames=list(params='a', year=2000:2005, iter=1))
flq <- FLQuant(3, dimnames=list(year=2000:2005))
flp %*% flq
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1 2 3 4 5 6
#>   all 6 6 6 6 6 6
#> 
#> units:  NA 

# Divide each FLQuants element by a 'param' in FLPar, e.g. time series
# divide by reference points
FLQuants(SSB=FLQuant(2303), F=FLQuant(0.8)) / FLPar(SSB=1560, F=0.4)
#> $ SSB 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1   
#>   all 1.48
#> 
#> units:  NA 
#> 
#> $ F 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1
#>   all 2
#> 
#> units:  NA / NA 
#> 

# Product of each FLQuants element by a 'param' in FLPar
FLQuants(SSB=FLQuant(2303), F=FLQuant(0.8)) * FLPar(SSB=1560, F=0.4)
#> $ SSB 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1      
#>   all 3592680
#> 
#> units:  NA 
#> 
#> $ F 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1   
#>   all 0.32
#> 
#> units:  NA 
#> 
# Divide each FLQuants element by each in FLPars
FLQuants(A=FLQuant(2303), B=FLQuant(1287)) /
  FLPars(A=FLPar(SBMSY=1560), B=FLPar(SBMSY=1000))
#> $ A 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1   
#>   all 1.48
#> 
#> units:  NA 
#> 
#> $ B 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1   
#>   all 1.29
#> 
#> units:  NA 
#> 
# Divide each FLQuants element by each in FLPars
FLQuants(A=FLQuant(2303), B=FLQuant(1287)) *
  FLPars(A=FLPar(SBMSY=1560), B=FLPar(SBMSY=1000))
#> $ A 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1      
#>   all 3592680
#> 
#> units:  NA 
#> 
#> $ B 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1      
#>   all 1287000
#> 
#> units:  NA 
#> 
# Divide each FLQuants element by each in FLPars
FLQuants(A=FLQuant(300), B=FLQuant(200)) /
  FLQuants(A=FLQuant(3), B=FLQuant(2))
#> $ A 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1  
#>   all 100
#> 
#> units:  NA 
#> 
#> $ B 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1  
#>   all 100
#> 
#> units:  NA 
#> 
# Divide each FLQuants element by each in FLPars
FLQuants(A=FLQuant(100), B=FLQuant(200)) *
  FLQuants(A=FLQuant(3), B=FLQuant(2))
#> $ A 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1  
#>   all 300
#> 
#> units:  NA 
#> 
#> $ B 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1  
#>   all 400
#> 
#> units:  NA 
#> 
# Divide each FLQuants element by each in FLPars
FLQuants(A=FLQuant(100), B=FLQuant(200)) *
  FLQuants(A=FLQuant(3), B=FLQuant(2))
#> $ A 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1  
#>   all 300
#> 
#> units:  NA 
#> 
#> $ B 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1  
#>   all 400
#> 
#> units:  NA 
#> 
# Divide each FLQuants element by each in FLPars
FLQuants(A=FLQuant(100), B=FLQuant(200)) *
  FLQuants(A=FLQuant(3), B=FLQuant(2))
#> $ A 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1  
#>   all 300
#> 
#> units:  NA 
#> 
#> $ B 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1  
#>   all 400
#> 
#> units:  NA 
#> 
```

</div>

</div>

</div>
