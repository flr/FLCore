<div id="main" class="col-md-9" role="main">

# Calculate the survivors of a stock to the next year.

<div class="ref-description section level2">

An FLStock object containing estimates of adundance at age ('stock.n')
and harvest level at age ('harvest'), is used to bring forward the
population by applying the total mortality at age ('z'). No calculation
is made on recruitment, so abundances for the first age will be set as
'NA', unless a value is provided.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
survivors(object, rec = NA)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    An FLStock with estimated harvest and abundances

-   rec:

    Value for recruitment, first age abundance, 'numeric' or 'FLQuant'.'

</div>

<div class="section level2">

## Value

The abundances at age of the survivors, 'FLQuant'.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data(ple4)
stock.n(ple4[, ac(2002:2006)])
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  2002    2003    2004    2005    2006   
#>   1  1792880  557844 1235790  863893  875191
#>   2   481832 1362670  417612  894667  618023
#>   3   413685  272903  729522  238706  552652
#>   4   181506  185801  118238  365617  136058
#>   5    98490   90506   97419   66113  219194
#>   6   106479   49763   49696   57108   41505
#>   7    13278   51719   26044   29565   37778
#>   8     7752    7046   28479   16720   21234
#>   9     3675    4964    4649   20534   12942
#>   10    9309    9758   11424   12765   27199
#> 
#> units:  1000 
survivors(ple4[, ac(2002:2006)])
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  2003    2004    2005    2006    2007   
#>   1       NA      NA      NA      NA      NA
#>   2  1362673  417612  894668  618023  651007
#>   3   272904  729521  238706  552652  393107
#>   4   185801  118238  365617  136058  326369
#>   5    90506   97419   66113  219193   87129
#>   6    49763   49696   57108   41505  148406
#>   7    51719   26044   29565   37778   29246
#>   8     7046   28479   16720   21234   28311
#>   9     4964    4649   20534   12942   17026
#>   10    9758   11424   12765   27199   33875
#> 
#> units:  1000 
```

</div>

</div>

</div>
