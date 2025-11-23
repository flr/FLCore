<div id="main" class="col-md-9" role="main">

# Returns the first and last parts of an FLQuant.

<div class="ref-description section level2">

Standard tail and head methods can be applied along any dimension of an
FLQuant object.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLQuant'
tail(x, n = 1, dim = 2, ...)

# S4 method for class 'FLQuant'
head(x, n = 1, dim = 2, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    The object to extract from, FLQuant.

-   n:

    The number of elements to extract, numeric.

-   dim:

    Dimension to extract from, defaults to 2, 'year'.

</div>

<div class="section level2">

## Value

An FLQuant with the extracted elements.

</div>

<div class="section level2">

## See also

<div class="dont-index">

base::tail

</div>

</div>

<div class="section level2">

## Author

Iago Mosqueira (WMR)

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
x <- FLQuant(1:10)

# Extract the last 3 years
tail(x, 3)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 8  9  10
#>   all  8  9 10
#> 
#> units:  NA 

# Extract all but the first 3 years
tail(x, -3)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 4  5  6  7  8  9  10
#>   all  4  5  6  7  8  9 10
#> 
#> units:  NA 

# Extract the first 3 years
head(x, 3)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1  2  3  4  5  6  7  8  9  10
#>   all  1  2  3  4  5  6  7  8  9 10
#> 
#> units:  NA 

# Extract all but the last 3 years
head(x, -3)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>       year
#> quant  1 2 3 4 5 6 7 8 9 10
#> 
#> units:  NA 
```

</div>

</div>

</div>
