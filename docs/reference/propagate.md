<div id="main" class="col-md-9" role="main">

# Method propagate

<div class="ref-description section level2">

Methods to extend objects of various FLR classes along the `iter` (6th
FLQuant) dimension. Objects must generally have a single `iter` to be
extended. The new iterations can be filled with copies of the existing,
or remain as `NA`.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
propagate(object, ...)

# S4 method for class 'FLQuant'
propagate(object, iter, fill.iter = TRUE)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    Object to be propagated.

-   fill.iter:

    Should first array be copied to others? Defaults to FALSE.

-   iters:

    No. of iterations in output.

</div>

<div class="section level2">

## Generic function

propagate(object, ...)

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLQuant](FLQuant.md)

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
# An FLQuant with one iter (dim(flq)[6] == 1)
flq <- FLQuant(rnorm(80), dim=c(4,20), quant='age')

# can now be extended along the `iter` dimension, with
#' copies of the first
propagate(flq, 100)
#> An object of class "FLQuant"
#> iters:  100 
#> 
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 1          2          3          4          5          6         
#>   1  0.3631(0)  1.3423(0) -0.7264(0)  0.0599(0) -0.0681(0) -1.0873(0)
#>   2 -1.3249(0)  0.1910(0) -0.5390(0) -1.3080(0) -0.0247(0) -1.4052(0)
#>   3 -2.1890(0)  1.2203(0) -0.5486(0) -1.5508(0)  0.2786(0) -1.4771(0)
#>   4 -0.2155(0)  0.8316(0) -0.8987(0) -0.8991(0)  1.0162(0)  0.7555(0)
#>    year
#> age 7          8          9          10         11         12        
#>   1 -1.7830(0) -0.9464(0)  1.1726(0) -0.3175(0)  0.3159(0)  0.3558(0)
#>   2 -0.8466(0) -0.6997(0) -2.2825(0)  1.6480(0)  0.7384(0)  2.2736(0)
#>   3 -1.1510(0) -0.1039(0) -0.6594(0)  3.1612(0) -0.3775(0) -0.1425(0)
#>   4  1.2335(0)  0.2311(0) -0.3462(0)  0.4604(0)  0.7628(0)  0.0575(0)
#>    year
#> age 13         14         15         16         17         18        
#>   1 -0.0212(0)  0.7195(0)  0.1823(0)  0.3909(0) -0.3831(0) -0.9798(0)
#>   2 -0.7765(0)  2.0010(0) -0.2446(0) -0.2496(0) -0.1301(0) -1.0690(0)
#>   3  0.5277(0) -0.3680(0)  1.1278(0) -1.1934(0) -1.2186(0) -0.1634(0)
#>   4  0.9934(0) -0.9506(0) -1.3992(0)  1.5209(0)  1.0802(0)  1.5960(0)
#>    year
#> age 19         20        
#>   1  0.4713(0) -0.5259(0)
#>   2  0.3833(0)  1.6269(0)
#>   3  0.1576(0) -0.8346(0)
#>   4 -1.3149(0)  0.6855(0)
#> 
#> units:  NA 

# or without
iter(propagate(flq, 100, fill.iter=FALSE), 2)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20
#>   1 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#>   2 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#>   3 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#>   4 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#> 
#> units:  NA 
```

</div>

</div>

</div>
