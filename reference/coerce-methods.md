# Convert Objects Between Classes

Objects of various **FLCore** classes can be converted into other
classes, both basic R ones, like `data.frame`, and others defined in the
package. For the specifics of the precise calculations carried out for
each pair of classes, see below.

## Usage

``` r
# S4 method for class 'FLArray,data.frame'
coerce(from, to = "data.frame", strict = TRUE)

# S4 method for class 'FLPar,data.frame'
coerce(from, to = "data.frame", strict = TRUE)

# S4 method for class 'data.frame,FLQuant'
coerce(from, to = "FLQuant", strict = TRUE)

# S4 method for class 'FLBiol,FLStock'
coerce(from, to = "FLStock", strict = TRUE)

# S4 method for class 'data.frame,FLStock'
coerce(from, to = "FLStock", strict = TRUE)

# S4 method for class 'FLStockLen,FLStock'
coerce(from, to = "FLStock", strict = TRUE)

# S4 method for class 'FLComp,FLQuants'
coerce(from, to = "FLQuants", strict = TRUE)

# S4 method for class 'FLBiol,FLSR'
coerce(from, to = "FLSR", strict = TRUE)

# S4 method for class 'FLSR,predictModel'
coerce(from, to = "predictModel", strict = TRUE)
```

## Arguments

- object:

  Object to be converted.

- Class:

  Name of the class to convert the object to, `character`.

## Value

An object of the requested class.

## FLArray to data.frame

The six dimensions of an `FLArray` are converted into seven columns,
named `quant` (or any other name given to the first dimension in the
object), `year`, `unit`, `season`, `area`, `iter` and `data`. The last
one contains the actual numbers stored in the array. `units` are stored
as an attribute to the `data.frame`. The `year` and `data` columns are
of type `numeric`, while all others are `factor`.

## FLPar to data.frame

The two or more dimensions of an *FLPar* objects are converted into
three or more columns. For a 2D objects, they are named *params*, *iter*
and *data*. The last one contains the actual numbers stored in the
array, in a column type `numeric`, while all others are `factor`.

## data.frame to FLQuant

A *data.frame* with the right column names is converted into an
*FLQuant* object with missing values being added. Missing columns are
assumed to contain the default dimnames in *FLQuant*.

## *FLBiol* to *FLStock*

- *n* = *stock.n*

- *wt* = *stock.wt*

- *m* = *m*

- *mat* = *mat()*

- *m.spwm*, *harvest.spwn* = *spwn*

## *data.frame* to *FLStock*

A *data.frame* with the right column names is converted into an
*FLStock* object with missing values being added.

## *FLStockLen* to *FLStock*

- Changes quant from 'len' to 'age'

## FLComp to FLQuant

A *data.frame* with the right column names is converted into an
*FLQuant*

## FLBiol to FLSR

Slots `rec` and `ssb` in `FLSR` are populated from `n[1,]` and the
result of [`ssb()`](ssb.md), adjusted from recruitment age. Note
`ssb(FLBiol)` only corrects for natural mortality (`m`) to time of
spanwing (`spwn`).

## FLSR to predictModel

Places the `model` and `params` slots the same-named slots of
`predictModel`. The `residuals` `FLQuant` is stored under that name in
the base `FLQuants` slot.

## See also

base::as, base::coerce

## Author

The FLR Team

## Examples

``` r
# from FLQuant to data.frame
as(FLQuant(rnorm(100), dim=c(5, 20)), "data.frame")
#>     quant year   unit season   area iter         data
#> 1       1    1 unique    all unique    1  0.725298945
#> 2       2    1 unique    all unique    1  0.195409250
#> 3       3    1 unique    all unique    1 -0.413650614
#> 4       4    1 unique    all unique    1 -1.328259249
#> 5       5    1 unique    all unique    1  1.730665919
#> 6       1    2 unique    all unique    1  0.187037560
#> 7       2    2 unique    all unique    1 -0.935728727
#> 8       3    2 unique    all unique    1  0.083062326
#> 9       4    2 unique    all unique    1  0.160518049
#> 10      5    2 unique    all unique    1  0.202105749
#> 11      1    3 unique    all unique    1 -1.995176929
#> 12      2    3 unique    all unique    1 -0.227655067
#> 13      3    3 unique    all unique    1 -0.010629297
#> 14      4    3 unique    all unique    1  0.576946108
#> 15      5    3 unique    all unique    1 -0.309330062
#> 16      1    4 unique    all unique    1 -0.361320050
#> 17      2    4 unique    all unique    1 -1.178973937
#> 18      3    4 unique    all unique    1 -0.351561307
#> 19      4    4 unique    all unique    1  0.682996726
#> 20      5    4 unique    all unique    1  0.396960455
#> 21      1    5 unique    all unique    1  1.103677767
#> 22      2    5 unique    all unique    1  0.283071115
#> 23      3    5 unique    all unique    1 -0.119032853
#> 24      4    5 unique    all unique    1 -0.850775902
#> 25      5    5 unique    all unique    1 -0.004881436
#> 26      1    6 unique    all unique    1 -0.555004527
#> 27      2    6 unique    all unique    1 -0.005464035
#> 28      3    6 unique    all unique    1  0.596582274
#> 29      4    6 unique    all unique    1  0.930140787
#> 30      5    6 unique    all unique    1 -1.545631788
#> 31      1    7 unique    all unique    1 -0.393499692
#> 32      2    7 unique    all unique    1  1.127952598
#> 33      3    7 unique    all unique    1 -0.426971110
#> 34      4    7 unique    all unique    1  0.945087699
#> 35      5    7 unique    all unique    1 -1.527816578
#> 36      1    8 unique    all unique    1  0.359213899
#> 37      2    8 unique    all unique    1 -0.857820367
#> 38      3    8 unique    all unique    1  0.810534835
#> 39      4    8 unique    all unique    1 -0.219024446
#> 40      5    8 unique    all unique    1  0.008369587
#> 41      1    9 unique    all unique    1 -0.290741234
#> 42      2    9 unique    all unique    1 -0.215558718
#> 43      3    9 unique    all unique    1  0.621358173
#> 44      4    9 unique    all unique    1 -0.913926411
#> 45      5    9 unique    all unique    1  0.313734983
#> 46      1   10 unique    all unique    1  0.879905108
#> 47      2   10 unique    all unique    1 -0.351130640
#> 48      3   10 unique    all unique    1 -1.824247200
#> 49      4   10 unique    all unique    1 -1.279339877
#> 50      5   10 unique    all unique    1 -1.592609993
#> 51      1   11 unique    all unique    1 -1.472094029
#> 52      2   11 unique    all unique    1  0.868931876
#> 53      3   11 unique    all unique    1  0.407377006
#> 54      4   11 unique    all unique    1 -0.949259933
#> 55      5   11 unique    all unique    1 -0.086388772
#> 56      1   12 unique    all unique    1 -0.811054239
#> 57      2   12 unique    all unique    1  0.221324492
#> 58      3   12 unique    all unique    1 -1.023152353
#> 59      4   12 unique    all unique    1 -0.004465181
#> 60      5   12 unique    all unique    1  0.880233578
#> 61      1   13 unique    all unique    1 -1.982349310
#> 62      2   13 unique    all unique    1 -0.585358329
#> 63      3   13 unique    all unique    1 -1.845920215
#> 64      4   13 unique    all unique    1 -0.347057895
#> 65      5   13 unique    all unique    1  1.689775598
#> 66      1   14 unique    all unique    1  0.379050795
#> 67      2   14 unique    all unique    1 -0.459959239
#> 68      3   14 unique    all unique    1  0.767678135
#> 69      4   14 unique    all unique    1  1.707361650
#> 70      5   14 unique    all unique    1  1.131363415
#> 71      1   15 unique    all unique    1  0.890468643
#> 72      2   15 unique    all unique    1  1.881834283
#> 73      3   15 unique    all unique    1 -0.642540707
#> 74      4   15 unique    all unique    1  0.837664328
#> 75      5   15 unique    all unique    1 -0.567759708
#> 76      1   16 unique    all unique    1  0.535235735
#> 77      2   16 unique    all unique    1 -0.530982732
#> 78      3   16 unique    all unique    1  0.729320408
#> 79      4   16 unique    all unique    1 -0.552114798
#> 80      5   16 unique    all unique    1  0.031801214
#> 81      1   17 unique    all unique    1 -1.891338980
#> 82      2   17 unique    all unique    1 -0.056759478
#> 83      3   17 unique    all unique    1  0.865373596
#> 84      4   17 unique    all unique    1  0.220081461
#> 85      5   17 unique    all unique    1 -0.595956703
#> 86      1   18 unique    all unique    1 -0.122960377
#> 87      2   18 unique    all unique    1  0.078194024
#> 88      3   18 unique    all unique    1 -0.963582785
#> 89      4   18 unique    all unique    1 -1.253456258
#> 90      5   18 unique    all unique    1  0.512253919
#> 91      1   19 unique    all unique    1 -0.211692262
#> 92      2   19 unique    all unique    1 -0.745340417
#> 93      3   19 unique    all unique    1 -1.193234702
#> 94      4   19 unique    all unique    1  2.055037250
#> 95      5   19 unique    all unique    1  1.632423556
#> 96      1   20 unique    all unique    1 -0.532611267
#> 97      2   20 unique    all unique    1  0.023402388
#> 98      3   20 unique    all unique    1 -0.948465802
#> 99      4   20 unique    all unique    1  0.153762418
#> 100     5   20 unique    all unique    1 -1.588615779
# from FLPar to data.frame
as(FLPar(phi=rnorm(10), rho=rlnorm(10)), "data.frame")
#>    params iter        data
#> 1     phi    1  0.25186335
#> 2     rho    1  0.54177652
#> 3     phi    2 -0.65353669
#> 4     rho    2  0.39091841
#> 5     phi    3 -0.79287363
#> 6     rho    3  1.11073027
#> 7     phi    4  0.27292991
#> 8     rho    4  0.24271767
#> 9     phi    5  0.71286203
#> 10    rho    5  0.92988702
#> 11    phi    6  0.28297534
#> 12    rho    6  2.07908063
#> 13    phi    7  0.36346946
#> 14    rho    7  0.71074226
#> 15    phi    8  0.06014947
#> 16    rho    8  0.28901744
#> 17    phi    9 -0.81620199
#> 18    rho    9  4.53796306
#> 19    phi   10 -0.52723224
#> 20    rho   10  0.43943025
# from data.frame to FLQuant
as(data.frame(age=rep(1:4, each=3), year=2011:2013, data=rnorm(12)), "FLQuant")
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 2011   2012   2013  
#>   1 -0.448 -1.451 -1.073
#>   2  1.501  1.906  0.598
#>   3  1.298 -0.337  0.577
#>   4 -2.098  0.972  1.257
#> 
#> units:  NA 
# from FLBiol to FLStock
data(ple4.biol)
#> Warning: namespace ‘colorspace’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘ggplot2’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘Rcpp’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘dplyr’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘data.table’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘remotes’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘xtable’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘shiny’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘htmlwidgets’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘gtable’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘scales’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
fls <- as(ple4.biol, 'FLStock')
# from data.frame to FLStock
df <- data.frame(slot="m", age=rep(1:5, each=5), year=rep(2000:2004, 5),
  data=0.2, units="m")
fls <- as(df, 'FLStock')
# from data.frame to FLQuant
# from FLBiol to FLSR
as(ple4.biol, "FLSR")
#> An object of class "FLSR"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  1   60  1   1   1   1   
#> 
#> Range:  min  minyear max maxyear 
#>  1   1958    1   2017    
#> 
#> 
#> Model:   rec ~ a + ssb/ssb - 1
#> An object of class "FLPar"
#> params
#>       a 
#> 1119523 
#> units:  NA 
#> Log-likelihood:  NA(NA) 
#> Variance-covariance:       params
#> params  a
#>      a NA
# from FLSR to predictModel
data(nsher)
as(nsher, 'predictModel')
#> $ residuals 
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 1960     1961     1962     1963     1964     1965     1966     1967    
#>   0 -1.15378  0.96234 -0.00354  0.35803  0.56182 -0.23488 -0.49358 -0.13445
#>    year
#> age 1968     1969     1970     1971     1972     1973     1974     1975    
#>   0  0.14964 -0.45162  0.26880  0.26852 -0.22839 -0.79382  0.26790 -1.16366
#>    year
#> age 1976     1977     1978     1979     1980     1981     1982     1983    
#>   0 -1.15490 -0.22336 -0.45775 -0.08418  0.19285  0.66942  0.93046  0.58859
#>    year
#> age 1984     1985     1986     1987     1988     1989     1990     1991    
#>   0  0.22503  0.62943  0.82707  0.62975 -0.08775 -0.15797 -0.25362 -0.32134
#>    year
#> age 1992     1993     1994     1995     1996     1997     1998     1999    
#>   0  0.36452  0.33240 -0.11115  0.15238  0.35094 -0.36569 -0.47569  0.42681
#>    year
#> age 2000     2001     2002     2003     2004    
#>   0 -0.07039  0.70086 -0.03528 -0.72674 -0.67404
#> 
#> units:   
#> 
#> model:  
#> rec ~ a * ssb * exp(-b * ssb)
#> <environment: R_EmptyEnv>
#> 
#> params:  
#> An object of class "FLPar"
#> params
#>        a        b 
#> 1.19e+02 9.45e-03 
#> units:  NA 
```
