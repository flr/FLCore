# Class FLStockR

An `FLStock` extended with reference points and a stock-recruitment
relationship, allowing the computation of biological reference points
and stock depletion directly from the stock object.

## Usage

``` r
FLStockR(object, ...)
```

## Slots

- .Data:

  Inherited from `FLStock`.

- refpts:

  Biological reference points. `FLPar`.

- sr:

  Stock-recruitment relationship. `predictModel`.

## See also

[`FLStock`](FLStock.md), [`FLPar`](FLPar.md),
[`predictModel`](predictModel.md)

## Examples

``` r
data(ple4)
object <- FLStockR(ple4, refpts=FLPar(BMSY=150000, SB0=1.74e6),
  sr=predictModel(model=rec~a*ssb/(b+ssb), params=FLPar(a=1155334, b=56040)))
```
