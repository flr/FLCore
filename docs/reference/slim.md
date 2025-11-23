<div id="main" class="col-md-9" role="main">

# Drop unnecesary 'iters'

<div class="ref-description section level2">

Objects of FLR classes can vary in the length along the sixth dimension
in any slot of class [FLQuant](FLQuant.md). This reduces object size and
memory usage. If an object has been extended fully, for example by using
`propagate`, we can slim down the object by reducing any slot where all
iters are identical and keeping only yhe first *iter*.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
slim(object, ...)

# S4 method for class 'FLComp'
slim(object, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    A complex **FLR** object to slim down.

</div>

<div class="section level2">

## Value

An object of the same class as the input.

</div>

<div class="section level2">

## Details

The test for whether an slot can be slimmed is based on checking if the
sum of the variance along the 6th dimensions is equal to zero.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLQuant](FLQuant.md) `propagate`

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
data(ple4)
# Extend all of ple4 to 50 iters
ple4 <- propagate(ple4, 50)
# Add variability in catch.n
catch.n(ple4) <- rlnoise(50, log(catch.n(ple4)), log(catch.n(ple4))/10)
summary(ple4)
#> An object of class "FLStock"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  61  1   1   1   50  
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   10  10  1957    2017    2   6   
#> 
#> Metrics: 
#>   rec: 367450 - 4303680  (1000) 
#>   ssb: 203391 - 913290  (t) 
#>   catch: 78360 - 315245  (t) 
#>   fbar: 0.20 - 0.72  (f) 
# slim object by dropping identical iters
sple4 <- slim(ple4)
summary(sple4)
#> An object of class "FLStock"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  61  1   1   1   50  
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   10  10  1957    2017    2   6   
#> 
#> Metrics: 
#>   rec: 367450 - 4303680  (1000) 
#>   ssb: 203391 - 913290  (t) 
#>   catch: 78360 - 315245  (t) 
#>   fbar: 0.20 - 0.72  (f) 
```

</div>

</div>

</div>
