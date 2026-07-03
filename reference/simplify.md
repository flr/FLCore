# Aggregate or select along unwanted dimensions

Objects of many FLR classes might be aggregated along the "unit",
"season", and/or "area" dimensions according to the type of data they
contain.

Helper functions called by `simplify()` to collapse the *unit*,
*season*, or *area* dimension of an [FLStock](FLStock.md) object to a
single level. Numbers-at-age slots are summed across the collapsed
dimension; weight-at-age and natural mortality slots are computed as
abundance-weighted means. Aggregate biomass slots are recomputed after
collapsing.

## Usage

``` r
simplify(object, ...)

nounit(stock)

noseason(stock, spwn.season = 1, rec.season = spwn.season, weighted = FALSE)

noarea(stock)

# S4 method for class 'FLStock'
simplify(
  object,
  dims = c("unit", "season", "area")[dim(object)[3:5] > 1],
  spwn.season = 1,
  rec.season = spwn.season,
  harvest = TRUE,
  weighted = FALSE
)
```

## Arguments

- object:

  A complex **FLR** object to aggregate.

- stock:

  An object of class [FLStock](FLStock.md).

- spwn.season:

  Integer. Season used to set spawning timing slots (`harvest.spwn`,
  `m.spwn`) and to extract maturity. Defaults to `1`.

- rec.season:

  Integer. Season in which age-0 recruits first appear. Defaults to
  `spwn.season`.

- weighted:

  Logical. If `TRUE`, weight-at-age slots are collapsed as
  abundance-weighted means; otherwise simple means are used. Defaults to
  `FALSE`.

## Value

An object of the same class as the input.

An [FLStock](FLStock.md) with the relevant dimension reduced to length
1.

## Functions

- `noseason()`: Collapse the *season* dimension, summing catches and
  computing weighted-mean weights-at-age and total natural mortality.

- `noarea()`: Collapse the *area* dimension, summing catches and
  computing abundance-weighted means for weights-at-age and natural
  mortality.

## See also

`simplify()`

## Author

The FLR Team

## Examples

``` r
data(ple4)
# nounit: collapse a stock with a single unit (no-op)
nounit(ple4)
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

# noseason: collapse seasons (stock has 1 season, so no-op)
noseason(expand(ple4, season=1:4))
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
#>   catch: 313441 - 1260979  (t) 
#>   fbar: 0.20 - 0.72  (f) 
# noarea: collapse areas (stock has 1 area, so no-op)
noarea(ple4)
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
```
