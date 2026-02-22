# msy: A series of methods to extract or compute MSY-based reference points

Reference points based on equilibirum calculations of Maximum
Sustainable Yield (MSY) are computed by various FLR packages. The
methods' generics are defined here for convenience. Please refer to the
help pages of particular methods for further details

## Usage

``` r
msy(x, ...)

bmsy(x, ...)

sbmsy(x, ...)

fmsy(x, ...)
```

## Arguments

- x:

  An input object from which to extract or compute a reference point

## Value

A value for the requested reference point, 'FLPar'

## Details

The four methods provide the following parameter estimates:

- `msy` Maximum Sustainable Yield (MSY)

- `fmsy` Fishing mortality level expected to produce on average MSY

- `bmsy` Total biomass that should produce MSY

- `sbmsy` Spawning biomass that should produce MSY

## See also

[`FLPar`](FLPar.md)

## Author

The FLR Team
