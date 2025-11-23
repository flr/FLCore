<div id="main" class="col-md-9" role="main">

# FLQuant class for numerical data

<div class="ref-description section level2">

The `FLQuant` class is a six-dimensional `array` designed to store most
quantitative data used in fisheries and population modelling.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
FLQuant(object, ...)

# S4 method for class 'missing'
FLQuant(
  object,
  dim = rep(1, 6),
  dimnames = "missing",
  quant = NULL,
  units = "NA",
  iter = 1
)

# S4 method for class 'vector'
FLQuant(
  object,
  dim = rep(1, 6),
  dimnames = "missing",
  quant = NULL,
  units = "NA",
  iter = 1,
  fill.iter = TRUE
)

# S4 method for class 'array'
FLQuant(
  object,
  dim = rep(1, 6),
  dimnames = "missing",
  quant = NULL,
  units = "NA",
  iter = 1,
  fill.iter = TRUE
)

# S4 method for class 'matrix'
FLQuant(object, dim = lapply(dimnames, length), dimnames = "missing", ...)

# S4 method for class 'FLQuant'
FLQuant(
  object,
  quant = attributes(object)[["quant"]],
  units = attributes(object)[["units"]],
  dimnames = attributes(object)[["dimnames"]],
  iter = dim(object)[6],
  fill.iter = TRUE,
  dim = attributes(object)[["dim"]]
)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    Input numeric object

-   ...:

    Additional arguments

-   dim:

    Vector of dimension lengths

-   dimnames:

    List of dimension names

-   quant:

    Character vector for name of first dimension

-   units:

    Character vctor of units of measurement, see [uom](uom.md)

-   iter:

    Number of iterations, i.e. length of the 6th dimension

-   fill.iter:

    Should iterations be filled with the same content as the first?

</div>

<div class="section level2">

## Details

The six dimensions are named. The name of the first dimension can be
altered by the user from its default, `quant`. This could typically be
`age` or `length` for data related to natural populations. The only name
not accepted is 'cohort', as data structured along cohort should be
stored using the `FLCohort` class instead. Other dimensions are always
names as follows: `year`, for the calendar year of the datapoint;
`unit`, for any kind of division of the population, e.g. by sex;
`season`, for any temporal strata shorter than year; `area`, for any
kind of spatial stratification; and `iter`, for replicates obtained
through bootstrap, simulation or Bayesian analysis.

In addition, `FLQuant` objects contain a `units` attribute, of class
`character`, intended to contain the units of measurement relevant to
the data.

</div>

<div class="section level2">

## Slots

-   .Data:

    A 6-D array for numeric data. `array`.

-   units:

    Units of measurement. `character`.

</div>

<div class="section level2">

## Validity

-   Dimensions::

    Array must have 6 dimensions

-   Content::

    Array must be of class `numeric`

-   Dimnames::

    Dimensions 2 to 6 must be named "year", "unit", "season", "area" and
    "iter"

</div>

<div class="section level2">

## Constructor

The `FLQuant` method provides a flexible constructor for objects of the
class. Inputs can be of class:

-   `vector`::

    A numeric vector will be placed along the year dimension by default.

-   `matrix`::

    A matrix will be placed along dimensions 1 and 2, unless otherwise
    specified by 'dim'. The matrix dimnames will be used unless
    overriden by 'dimnames'.

-   [array](https://rdrr.io/r/base/array.html)::

    As above

-   [missing](https://rdrr.io/r/base/missing.html)::

    If no input is given, an empty `FLQuant` (NA) is returned, but
    dimensions and dimnames can still be specified.

Additional arguments to the constructor:

-   units::

    The units of measurement, a `character` string.

-   dim::

    The dimensions of the object, a `numeric` vector of length 6.

-   dimnames::

    A `list` object providing the dimnames of the array. Only those
    different from the default ones need to be specified.

-   quant::

    The name of the first dimension, if different from 'quant', as a
    `character` string.

</div>

<div class="section level2">

## See also

<div class="dont-index">

`FLQuant`

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
# creating a new FLQuant
flq <- FLQuant()
flq <- FLQuant(1:10, dim=c(2,5))
summary(flq)
#> An object of class "FLQuant" with:
#> 
#> dim:    quant year unit season area iter 
#>         2     5    1    1      1    1 
#> units:  NA 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    %NAs 
#>     1.0     3.2     5.5     5.5     7.8    10.0     0.0 

# Vectors are used column first...
dim(FLQuant(1:10))
#> [1]  1 10  1  1  1  1
# ...while matrices go row first.
dim(FLQuant(matrix(1:10)))
#> [1] 10  1  1  1  1  1

FLQuant(matrix(rnorm(100), ncol=20))
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1       2       3       4       5       6       7       8       9      
#>     1  0.9151  0.2644  0.2206 -0.4697  0.1106 -0.1496 -0.7064 -0.3692 -1.3664
#>     2  1.0786  1.0246 -1.4811 -0.2805  0.2742  0.2662 -1.2754  1.2124 -1.2981
#>     3  1.4528 -0.6592  0.7247  0.5748  1.5749  0.5284  0.7333 -0.4454 -2.1371
#>     4 -1.6533 -0.3863 -2.3775 -0.2057  0.5678 -0.5794 -0.4202  1.1387 -0.0962
#>     5 -2.7619 -0.2425 -0.0654  0.2349 -0.0596  0.9422 -0.2554  0.1905  0.6945
#>      year
#> quant 10      11      12      13      14      15      16      17      18     
#>     1 -0.4746 -0.8338 -1.9995 -0.0323 -0.4278  0.7408  0.2503 -0.1726  0.4613
#>     2  1.7631  0.3983 -0.9959 -0.7190 -2.0310 -1.2708 -1.7056 -1.2361  1.3814
#>     3  2.0258 -1.4745  0.4201 -1.1166  2.7508 -0.1636 -0.8554 -1.9023 -0.4165
#>     4  0.4276 -0.3954 -0.0629 -0.7803  1.5137 -0.6109 -0.1449 -0.0945  0.6809
#>     5 -0.5665 -0.1307 -0.0801 -1.7770  0.0340  1.2519 -0.3244  0.0326 -0.4144
#>      year
#> quant 19      20     
#>     1 -0.5183 -0.6087
#>     2 -0.6840 -0.7311
#>     3 -0.8856  2.7151
#>     4  0.0492 -1.3394
#>     5  0.1856 -0.6460
#> 
#> units:  NA 

FLQuant(array(rnorm(100), dim=c(5,2,1,1,1,10)))
#> An object of class "FLQuant"
#> iters:  10 
#> 
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1              2             
#>     1 -0.1277(1.186) -0.6769(0.631)
#>     2 -0.2801(0.755) -0.1837(0.925)
#>     3 -0.1080(0.406) -0.0535(1.260)
#>     4  0.3465(0.880)  0.2246(1.337)
#>     5 -0.5708(1.013) -0.2596(1.579)
#> 
#> units:  NA 
FLQuant(array(rnorm(100), dim=c(5,2)), iter=10)
#> An object of class "FLQuant"
#> iters:  10 
#> 
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1          2         
#>     1  0.0894(0) -0.9517(0)
#>     2  0.3245(0) -0.5586(0)
#>     3  0.0713(0)  0.2558(0)
#>     4  0.2330(0) -0.1981(0)
#>     5  1.9864(0)  0.4288(0)
#> 
#> units:  NA 

# working with FLQuant objects
flq <- FLQuant(rnorm(200), dimnames=list(age=1:5, year=2000:2008), units='diff')
summary(flq)
#> An object of class "FLQuant" with:
#> 
#> dim:    age year unit season area iter 
#>         5   9    1    1      1    1 
#> units:  diff 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    %NAs 
#>  -1.690  -0.528   0.048   0.135   0.700   2.586   0.000 

flq[1,]
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 2000    2001    2002    2003    2004    2005    2006    2007    2008   
#>   1 -0.4703  0.0482 -0.0801 -0.7793 -0.4247  1.9157  0.4295 -0.4247  2.5858
#> 
#> units:  diff 
flq[,1]
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 2000  
#>   1 -0.470
#>   2 -1.690
#>   3 -0.945
#>   4 -0.343
#>   5  0.358
#> 
#> units:  diff 
flq[1,1] <- 0

units(flq)
#> [1] "diff"
quant(flq)
#> [1] "age"

plot(flq)



FLQuant()
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1 
#>   all NA
#> 
#> units:  NA 
summary(FLQuant())
#> An object of class "FLQuant" with:
#> 
#> dim:    quant year unit season area iter 
#>         1     1    1    1      1    1 
#> units:  NA 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    %NAs 
#>      NA      NA      NA      NA      NA      NA     100 

FLQuant(1:10)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1  2  3  4  5  6  7  8  9  10
#>   all  1  2  3  4  5  6  7  8  9 10
#> 
#> units:  NA 

FLQuant(array(rnorm(9), dim=c(3,3,3)))
#> An object of class "FLQuant"
#> , , unit = 1, season = all, area = unique
#> 
#>      year
#> quant 1       2       3      
#>     1 -1.5556 -0.1294 -0.1934
#>     2  0.0935  0.4078 -0.2695
#>     3 -0.3669 -0.5840  0.0737
#> 
#> , , unit = 2, season = all, area = unique
#> 
#>      year
#> quant 1       2       3      
#>     1 -1.5556 -0.1294 -0.1934
#>     2  0.0935  0.4078 -0.2695
#>     3 -0.3669 -0.5840  0.0737
#> 
#> , , unit = 3, season = all, area = unique
#> 
#>      year
#> quant 1       2       3      
#>     1 -1.5556 -0.1294 -0.1934
#>     2  0.0935  0.4078 -0.2695
#>     3 -0.3669 -0.5840  0.0737
#> 
#> units:  NA 
FLQuant(matrix(rnorm(12), nrow=4, ncol=3))
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1       2       3      
#>     1  0.3572 -1.0497  0.4350
#>     2  0.5504  2.0520  0.5488
#>     3  0.0384  0.1764  0.6474
#>     4 -1.6096  1.1283  0.8785
#> 
#> units:  NA 

FLQuant(FLQuant(array(rnorm(9), dim=c(3,3,3)), units='kg'), units='t')
#> An object of class "FLQuant"
#> , , unit = 1, season = all, area = unique
#> 
#>      year
#> quant 1        2        3       
#>     1  0.35080 -0.28173 -1.18753
#>     2  0.04988 -0.79168  0.36242
#>     3  0.83575  0.00165 -0.54944
#> 
#> , , unit = 2, season = all, area = unique
#> 
#>      year
#> quant 1        2        3       
#>     1  0.35080 -0.28173 -1.18753
#>     2  0.04988 -0.79168  0.36242
#>     3  0.83575  0.00165 -0.54944
#> 
#> , , unit = 3, season = all, area = unique
#> 
#>      year
#> quant 1        2        3       
#>     1  0.35080 -0.28173 -1.18753
#>     2  0.04988 -0.79168  0.36242
#>     3  0.83575  0.00165 -0.54944
#> 
#> units:  t 
```

</div>

</div>

</div>
