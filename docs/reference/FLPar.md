<div id="main" class="col-md-9" role="main">

# Class FLPar

<div class="ref-description section level2">

A class for storing parameters of a model.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
FLPar(object, ...)
```

</div>

</div>

<div class="section level2">

## Details

The `FLPar` class is based on the array class which can store Monte
Carlo samples and the names of the relevant parameter vectors.

Methods for this class include subsetting and replacement as for the
`FLQuant` class. There are methods for extracting statistics of the
sample (mean, median etc.) and for plotting the parameter samples.

</div>

<div class="section level2">

## Slots

-   .Data:

    Describe slot. `array`.

-   units:

    Units of measurement. `character`.

</div>

<div class="section level2">

## See also

<div class="dont-index">

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
FLPar(rnorm(4), params=c('a','b','c','sigma2'))
#> An object of class "FLPar"
#> params
#>       a       b       c  sigma2 
#>  0.0178 -0.8595  1.3577  1.2151 
#> units:  NA NA NA NA 

FLPar(rnorm(20), dimnames=list(params=c('a','b'), year=1990:1999, iter=1),
  units='NA')
#> An object of class "FLPar"
#>       year
#> params 1990    1991    1992    1993    1994    1995    1996    1997    1998   
#>      a  1.4539 -0.6176 -1.3260 -1.4100  0.2959 -0.1027  0.8786  0.0903  0.5494
#>      b -0.0859 -0.2183 -2.3622  0.2544  0.0568  1.9519 -1.3066  0.9024 -0.8698
#>       year
#> params 1999   
#>      a -0.0392
#>      b -0.5185
#> units:  NA 

# with iters
  FLPar(rnorm(80), params=c('a', 'b'), iter=1:40)
#> An object of class "FLPar"
#> iters:  40 
#> 
#> params
#>              a              b 
#> 0.25352(0.837) 0.12703(1.562) 
#> units:  NA NA 
```

</div>

</div>

</div>
