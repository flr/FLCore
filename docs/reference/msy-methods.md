<div id="main" class="col-md-9" role="main">

# msy: A series of methods to extract or compute MSY-based reference points

<div class="ref-description section level2">

Reference points based on equilibirum calculations of Maximum
Sustainable Yield (MSY) are computed by various FLR packages. The
methods' generics are defined here for convenience. Please refer to the
help pages of particular methods for further details

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
msy(x, ...)

bmsy(x, ...)

sbmsy(x, ...)

fmsy(x, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    An input object from which to extract or compute a reference point

</div>

<div class="section level2">

## Value

A value for the requested reference point, 'FLPar'

</div>

<div class="section level2">

## Details

The four methods provide the following parameter estimates:

-   `msy` Maximum Sustainable Yield (MSY)

-   `fmsy` Fishing mortality level expected to produce on average MSY

-   `bmsy` Total biomass that should produce MSY

-   `sbmsy` Spawning biomass that should produce MSY

</div>

<div class="section level2">

## See also

<div class="dont-index">

`FLPar`

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

</div>
