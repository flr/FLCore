# Evaluates a predictModel slot inside the object cointaining it

Models in objects of the [predictModel](predictModel.md) class can make
use of slots and methods of the FLR class in which it is contained as a
slot. This function can be used by methods wishing to evaluate a single
`predictModel` slot in the context of the class it is part of.

## Usage

``` r
evalPredictModel(object, slot, ...)
```

## Arguments

- object:

  The FLR S4 over whicvh the predictModel evaluation should take place

- slot:

  The predictModel object to be evaluated

## Value

The result of evaluating the model, usually an `FLQuant`

## See also

[predictModel](predictModel.md)

## Author

The FLR Team
