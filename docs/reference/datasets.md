<div id="main" class="col-md-9" role="main">

# FLCore datasets

<div class="ref-description section level2">

Example datasets for the classes defined in FLCore.

</div>

<div class="section level2">

## Details

-   `ple4`, `FLStock`A dataset for North Sea (ICES Area IV) plaice.
    Catch, landings, discards, natural mortality, weight-at-age and
    maturity, together with the VPA estimated abundances and fishing
    mortalities.

-   `ple4sex`, `FLStock`A dataset of North Sea (ICES Area IV) plaice
    disaggregated by sex. Catch, yield, landings, discards, natural
    mortality, weight-at-age and maturity, together with the VPA
    estimated abundances and fishing mortalities.

-   ple4.index, `FLIndex`A dataset of North Sea (ICES Area IV) plaice
    survey catch per unit effort, index and index variance.

-   ple4.indices, `FLIndices`A dataset of three North Sea (ICES Area IV)
    plaice survey catch per unit effort series. Index and index
    variance.

-   ple4.biol, `FLBiol`A dataset of the North Sea plaice population.
    Numbers, natural mortality, mass and fecundity-at-age.

-   nsher , `FLSR`Stock and recruit data and fitted relationship for
    autumn spawning North Sea herring.

Datasets can be loaded by issuing the `data` command, like in
`data(ple4)`.

</div>

<div class="section level2">

## References

ICES.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLStock](FLStock.md), [FLSR](FLSR.md), [FLIndex](FLIndex.md),
[FLStock](FLStock.md), [FLIndex](FLIndex.md), [FLBiol](FLBiol.md)

</div>

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data(ple4)
summary(ple4)
#> An object of class "FLStock"
#> 
#> Name: PLE 
#> Description: Plaice in IV. ICES WGNSSK 2018. FLAAP 
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  61  1   1   1   1   
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   10  10  1957    2017    2   6   
#> 
#> Metrics: 
#>   rec: 367450 - 4303680  (1000) 
#>   ssb: 203391 - 913290  (t) 
#>   catch: 78360 - 315245  (t) 
#>   fbar: 0.20 - 0.72  (f) 

data(nsher)
is(nsher)
#> [1] "FLSR"    "FLModel" "FLComp" 
```

</div>

</div>

</div>
