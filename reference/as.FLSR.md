# Convert FLStock to FLSR

Create a stock-recruitment relationship object (FLSR) from an FLStock
object. The method extracts recruitment and spawning stock biomass (SSB)
data from an FLStock and constructs an FLSR object with properly lagged
time series.

## Usage

``` r
# S4 method for class 'FLStock'
as.FLSR(object, rec.age = dims(stock.n(object))$min, ...)
```

## Arguments

- object:

  An `FLStock` object to be converted

- rec.age:

  Integer specifying the age at which recruitment is measured. Defaults
  to the minimum age class in the stock. Must be at least the minimum
  age class of the FLStock.

- ...:

  Additional arguments to be passed to the `FLSR` constructor, including
  `model` to specify the stock-recruitment model formula.

## Value

An object of class `FLSR` containing:

- rec:

  Recruitment time series as FLQuant, lagged by rec.age years

- ssb:

  Spawning stock biomass time series as FLQuant

- model:

  Stock-recruitment model formula

- fitted:

  Empty FLQuant for fitted values

- residuals:

  Empty FLQuant for residuals

- name:

  Name inherited from the FLStock object

- desc:

  Description indicating origin from FLStock

## Details

The recruitment age (rec.age) is used to align the recruitment and SSB
time series. Recruitment at age `rec.age` in year t is paired with SSB
from year t-1. This ensures the stock-recruitment relationship reflects
the biological process where recruitment depends on the parent stock
biomass from the previous spawning season.

The time series are aligned as follows:

- Recruitment data: uses age rec.age from years (1 + rec.age) to end

- SSB data: uses years 1 to (total years - rec.age)

This ensures that recruitment observations are paired with the
corresponding parent stock biomass from the appropriate lag years.

An error is raised if:

- `rec.age` is less than the minimum age class

- The FLStock recruitment time series is too short (less than rec.age +
  1 years)

## See also

[`FLSR`](FLSR.md), [`as.FLSRs`](as.FLSRs.md), [`FLStock`](FLStock.md)

## Author

The FLR Team

## Examples

``` r
data(ple4)
# Create FLSR object with default recruitment age
sr <- as.FLSR(ple4, model=bevholt)

# Create FLSR object with recruitment age 2
sr2 <- as.FLSR(ple4, rec.age=2)

# Create FLSR with a specific model formula
sr_bh <- as.FLSR(ple4, model=rec~a*ssb/(b+ssb))
```
