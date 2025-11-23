<div id="main" class="col-md-9" role="main">

# which_iter

<div class="ref-description section level2">

Returns the positions along the 'iter' dimension that match a logical
estatement

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
which_iter(x, rule)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    An input object, class 'FLQuant'

-   rule:

    A logical 'rule', either as a 'character' or a logical 'FLQuant'

</div>

<div class="section level2">

## Value

As vector of positions along the 'iter' (6th) dimension

</div>

<div class="section level2">

## Details

DETAILS

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
# Creates an example object
x <- rnorm(50, FLQuant(runif(5, 10, 20)), 20)
# Looks for iters with a large value
which_iter(x, x > 60)
#> [1]  6 10
# rule as a character vector
which_iter(x, '> 60')
#> [1]  6 10
# rule has use & and |
which_iter(x, '> 50 & < 60')
#> [1]  4  9 13 16 18 28
which_iter(x, '< -40 | > 40')
#>  [1]  4  6  7  9 10 13 16 18 20 21 24 26 27 28 30 33 34 35 37 40 41 42 43 45 46
#> [26] 48 49 50
```

</div>

</div>

</div>
