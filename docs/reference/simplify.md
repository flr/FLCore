<div id="main" class="col-md-9" role="main">

# Aggregate or select along unwanted dimensions

<div class="ref-description section level2">

Objects of many FLR classes might be aggregated along the "unit",
"season", and/or "area" dimensions according to the type of data they
contain.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

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

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    A complex **FLR** object to aggregate.

</div>

<div class="section level2">

## Value

An object of the same class as the input.

</div>

<div class="section level2">

## Author

The FLR Team

</div>

</div>
