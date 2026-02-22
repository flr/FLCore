# Aggregate or select along unwanted dimensions

Objects of many FLR classes might be aggregated along the "unit",
"season", and/or "area" dimensions according to the type of data they
contain.

## Usage

``` r
simplify(object, ...)

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

## Value

An object of the same class as the input.

## Author

The FLR Team
