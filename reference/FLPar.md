# Class FLPar

A class for storing parameters of a model.

## Usage

``` r
FLPar(object, ...)
```

## Details

The `FLPar` class is based on the array class which can store Monte
Carlo samples and the names of the relevant parameter vectors.

Methods for this class include subsetting and replacement as for the
[`FLQuant`](FLQuant.md) class. There are methods for extracting
statistics of the sample (mean, median etc.) and for plotting the
parameter samples.

## Slots

- .Data:

  Describe slot. `array`.

- units:

  Units of measurement. `character`.

## See also

[\[](https://rdrr.io/r/base/Extract.html), \[\<-,
[as.data.frame](https://rdrr.io/r/base/as.data.frame.html),
[densityplot](https://rdrr.io/pkg/lattice/man/histogram.html),
[histogram](https://rdrr.io/pkg/lattice/man/histogram.html),
[iter](iter.md), iter\<-, [mean](https://rdrr.io/r/base/mean.html),
[median](https://rdrr.io/r/stats/median.html),
[plot](https://rdrr.io/r/graphics/plot.default.html),
[splom](https://rdrr.io/pkg/lattice/man/splom.html),
[summary](https://rdrr.io/r/base/summary.html),
[units,FLPar-method](units-FLCore.md), units\<-,FLPar,character-method,
[var](https://rdrr.io/r/stats/cor.html)

## Author

The FLR Team

## Examples

``` r
FLPar(rnorm(4), params=c('a','b','c','sigma2'))
#> An object of class "FLPar"
#> params
#>       a       b       c  sigma2 
#>  1.2404  0.6857 -0.0268  0.3096 
#> units:  NA NA NA NA 

FLPar(rnorm(20), dimnames=list(params=c('a','b'), year=1990:1999, iter=1),
  units='NA')
#> An object of class "FLPar"
#>       year
#> params 1990     1991     1992     1993     1994     1995     1996     1997    
#>      a  0.24986  0.59938  0.09072 -0.48178 -0.85954  1.21506 -0.08591 -0.21826
#>      b -1.35646  0.00865 -0.65706  0.01776  1.35770  1.45391 -0.61757 -1.32601
#>       year
#> params 1998     1999    
#>      a -2.36221  0.25440
#>      b -1.40997  0.29587
#> units:  NA 

# with iters
  FLPar(rnorm(80), params=c('a', 'b'), iter=1:40)
#> An object of class "FLPar"
#> iters:  40 
#> 
#> params
#>              a              b 
#> 0.12703(1.477) 0.20103(0.714) 
#> units:  NA NA 
```
