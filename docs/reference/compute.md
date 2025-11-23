<div id="main" class="col-md-9" role="main">

# Methods to compute quantities

<div class="ref-description section level2">

Methods to compute total quant-aggregated catch, landings, discards and
stock biomass from age or length-structured numbers and mean weights.

Methods to compute total quant-aggregated catch, landings, discards and
stock biomass from age or length-structured numbers and mean weights.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
computeLandings(object, ...)

computeDiscards(object, ...)

computeCatch(object, ...)

computeStock(object, ...)

computeHarvest(object, catch, ...)

computeLandings(object, ...)

computeDiscards(object, ...)

computeCatch(object, ...)

computeStock(object, ...)

# S4 method for class 'FLS'
computeLandings(object, na.rm = TRUE)

# S4 method for class 'FLS'
computeDiscards(object, na.rm = TRUE)

# S4 method for class 'FLS'
computeCatch(object, slot = c("catch", "n", "wt", "both", "all"), na.rm = TRUE)

# S4 method for class 'FLS'
computeStock(object, na.rm = TRUE)
```

</div>

</div>

<div class="section level2">

## Details

These methods compute the total catch, landings, discards and stock
biomass from the quant-structured values in numbers and weight per
individual. The calculation for landings, discards and stock involves
the product of the landings/discards/stock in numbers (`landings.n`,
`discards.n` or `stock.n`) by the individual weight-at-quant
(`landings.wt`, `discards.wt` or `stock.wt`), as in

$$L=L_n \* L\_{wt}$$

By selecting `slot="catch"`, `computeCatch` can calculate in the same
way the total catch from the catch-at-quant and weight in the catch.
Those two values (in slots `catch.n` and `catch.wt`) can also be
calculated (from landings and discards) by specifying `slot="n"` and
`slot="wt"` respectively. Calling `computeCatch` with option
`slot="all"` will carry out the three calculations. In this case, the
returned object will be of class `FLQuants`, with element names `catch`,
`catch.n` and `catch.wt`, which can then be passed directly to the
`catch<-` replacement method.

These methods compute the total catch, landings, discards and stock
biomass from the quant-structured values in numbers and weight per
individual. The calculation for landings, discards and stock involves
the product of the landings/discards/stock in numbers (`landings.n`,
`discards.n` or `stock.n`) by the individual weight-at-quant
(`landings.wt`, `discards.wt` or `stock.wt`), as in

$$L=L_n \* L\_{wt}$$

By selecting `slot="catch"`, `computeCatch` can calculate in the same
way the total catch from the catch-at-quant and weight in the catch.
Those two values (in slots `catch.n` and `catch.wt`) can also be
calculated (from landings and discards) by specifying `slot="n"` and
`slot="wt"` respectively. Calling `computeCatch` with option
`slot="all"` will carry out the three calculations. In this case, the
returned object will be of class `FLQuants`, with element names `catch`,
`catch.n` and `catch.wt`, which can then be passed directly to the
`catch<-` replacement method.

</div>

<div class="section level2">

## Generic function

computeCatch(object, ...)

computeLandings(object, ...)

computeDiscards(object, ...)

computeStock(object, ...)

computeCatch(object, ...)

computeLandings(object, ...)

computeDiscards(object, ...)

computeStock(object, ...)

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLComp](FLComp.md)

[FLComp](FLComp.md)

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
summary(computeLandings(ple4))
#> An object of class "FLQuant" with:
#> 
#> dim:    age year unit season area iter 
#>         1   61   1    1      1    1 
#> units:  t 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    %NAs 
#>   59713   82875  103162  108795  130443  187666       0 
summary(computeCatch(ple4, slot="all"))
#> An object of class "FLQuants"
#> 
#> Elements: catch.wt catch.n catch landings discards 
#> 
#> Name: catch.wt 
#>  dim  :  10 61 1 1 1 1 
#>  quant:  age 
#>  units:  kg 
#> 
#>  Min    :  0.02500463 
#>  1st Qu.:  0.2190491 
#>  Mean   :  0.4378413 
#>  Median :  0.4175921 
#>  3rd Qu.:  0.6279721 
#>  Max    :  1.126954 
#>  NAs    :  0 %
#> Name: catch.n 
#>  dim  :  10 61 1 1 1 1 
#>  quant:  age 
#>  units:  1000 
#> 
#>  Min    :  541.6833 
#>  1st Qu.:  7740.584 
#>  Mean   :  79380 
#>  Median :  32437.93 
#>  3rd Qu.:  114974.9 
#>  Max    :  1083226 
#>  NAs    :  0 %
#> Name: catch 
#>  dim  :  1 61 1 1 1 1 
#>  quant:  age 
#>  units:  t 
#> 
#>  Min    :  78360.36 
#>  1st Qu.:  131216.1 
#>  Mean   :  160583.9 
#>  Median :  149389.9 
#>  3rd Qu.:  175881.4 
#>  Max    :  315244.7 
#>  NAs    :  0 %
#> Name: landings 
#>  dim  :  1 61 1 1 1 1 
#>  quant:  age 
#>  units:  t 
#> 
#>  Min    :  59712.83 
#>  1st Qu.:  82874.98 
#>  Mean   :  108795.2 
#>  Median :  103161.8 
#>  3rd Qu.:  130443 
#>  Max    :  187666.1 
#>  NAs    :  0 %
#> Name: discards 
#>  dim  :  1 61 1 1 1 1 
#>  quant:  age 
#>  units:  t 
#> 
#>  Min    :  7434.188 
#>  1st Qu.:  35985.66 
#>  Mean   :  51788.75 
#>  Median :  46770.16 
#>  3rd Qu.:  61510.89 
#>  Max    :  153474.4 
#>  NAs    :  0 %
stock(ple4) <- computeStock(ple4)
landings(ple4) <- computeLandings(ple4)
catch.n(ple4) <- computeCatch(ple4, slot="n")
catch(ple4) <- computeCatch(ple4, slot="all")


data(ple4)
summary(computeLandings(ple4))
#> An object of class "FLQuant" with:
#> 
#> dim:    age year unit season area iter 
#>         1   61   1    1      1    1 
#> units:  t 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    %NAs 
#>   59713   82875  103162  108795  130443  187666       0 
summary(computeCatch(ple4, slot="all"))
#> An object of class "FLQuants"
#> 
#> Elements: catch.wt catch.n catch landings discards 
#> 
#> Name: catch.wt 
#>  dim  :  10 61 1 1 1 1 
#>  quant:  age 
#>  units:  kg 
#> 
#>  Min    :  0.02500463 
#>  1st Qu.:  0.2190491 
#>  Mean   :  0.4378413 
#>  Median :  0.4175921 
#>  3rd Qu.:  0.6279721 
#>  Max    :  1.126954 
#>  NAs    :  0 %
#> Name: catch.n 
#>  dim  :  10 61 1 1 1 1 
#>  quant:  age 
#>  units:  1000 
#> 
#>  Min    :  541.6833 
#>  1st Qu.:  7740.584 
#>  Mean   :  79380 
#>  Median :  32437.93 
#>  3rd Qu.:  114974.9 
#>  Max    :  1083226 
#>  NAs    :  0 %
#> Name: catch 
#>  dim  :  1 61 1 1 1 1 
#>  quant:  age 
#>  units:  t 
#> 
#>  Min    :  78360.36 
#>  1st Qu.:  131216.1 
#>  Mean   :  160583.9 
#>  Median :  149389.9 
#>  3rd Qu.:  175881.4 
#>  Max    :  315244.7 
#>  NAs    :  0 %
#> Name: landings 
#>  dim  :  1 61 1 1 1 1 
#>  quant:  age 
#>  units:  t 
#> 
#>  Min    :  59712.83 
#>  1st Qu.:  82874.98 
#>  Mean   :  108795.2 
#>  Median :  103161.8 
#>  3rd Qu.:  130443 
#>  Max    :  187666.1 
#>  NAs    :  0 %
#> Name: discards 
#>  dim  :  1 61 1 1 1 1 
#>  quant:  age 
#>  units:  t 
#> 
#>  Min    :  7434.188 
#>  1st Qu.:  35985.66 
#>  Mean   :  51788.75 
#>  Median :  46770.16 
#>  3rd Qu.:  61510.89 
#>  Max    :  153474.4 
#>  NAs    :  0 %
stock(ple4) <- computeStock(ple4)
landings(ple4) <- computeLandings(ple4)
catch.n(ple4) <- computeCatch(ple4, slot="n")
catch(ple4) <- computeCatch(ple4, slot="all")
```

</div>

</div>

</div>
