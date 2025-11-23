<div id="main" class="col-md-9" role="main">

# Evaluates a predictModel slot inside the object cointaining it

<div class="ref-description section level2">

Models in objects of the [predictModel](predictModel.md) class can make
use of slots and methods of the FLR class in which it is contained as a
slot. This function can be used by methods wishing to evaluate a single
`predictModel` slot in the context of the class it is part of.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
evalPredictModel(object, slot, ...)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    The FLR S4 over whicvh the predictModel evaluation should take place

-   slot:

    The predictModel object to be evaluated

</div>

<div class="section level2">

## Value

The result of evaluating the model, usually an `FLQuant`

</div>

<div class="section level2">

## See also

<div class="dont-index">

[predictModel](predictModel.md)

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

</div>
