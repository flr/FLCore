<div id="main" class="col-md-9" role="main">

# Method sweep for FLCore classes

<div class="ref-description section level2">

Use R's sweep method on FLCore classes

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLArray'
sweep(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...)

# S4 method for class 'FLPar'
sweep(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...)
```

</div>

</div>

<div class="section level2">

## Details

These methods call base R `sweep` method on **FLCore** classes and then
ensure that the returned object is of same class.

</div>

<div class="section level2">

## Generic function

sweep(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...)

</div>

<div class="section level2">

## See also

<div class="dont-index">

[sweep](https://rdrr.io/r/base/sweep.html)

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
flq <- FLQuant(rlnorm(90), dim=c(3,10), units='kg')
# Get ratio of max value by year
sweep(flq, 2, apply(flq, 2, max), "/")
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1      2      3      4      5      6      7      8      9      10    
#>     1 0.4883 0.2082 0.2685 0.0118 0.3853 1.0000 0.6724 1.0000 1.0000 1.0000
#>     2 1.0000 1.0000 1.0000 1.0000 1.0000 0.5733 0.5917 0.8930 0.5528 0.8618
#>     3 0.4154 0.1602 0.0190 0.0928 0.4219 0.1227 1.0000 0.2202 0.3669 0.1035
#> 
#> units:  kg 
```

</div>

</div>

</div>
