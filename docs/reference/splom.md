<div id="main" class="col-md-9" role="main">

# Method splom

<div class="ref-description section level2">

Draws a conditional scatter plot matrix.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLPar,missing'
splom(x, data, ...)
```

</div>

</div>

<div class="section level2">

## Details

See the help page in `lattice` for a full description of each plot and
all possible arguments.

</div>

<div class="section level2">

## Generic function

splom(x,data)

</div>

<div class="section level2">

## See also

<div class="dont-index">

[splom](https://rdrr.io/pkg/lattice/man/splom.html)

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
flp <- FLPar(c(t(mvrnorm(500, mu=c(0, 120, 0.01, 20),
  Sigma=matrix(.7, nrow=4, ncol=4) + diag(4) * 0.3))),
  dimnames=list(params=c('a','b','c','d'), iter=1:500), units="NA")

splom(flp)

```

</div>

</div>

</div>
