# Apply hyperstability or hyperdepletion to an abundance index

Modifies an abundance index to exhibit hyperstability (apparent
abundance remains high even as true abundance declines) or
hyperdepletion (apparent abundance drops faster than true abundance)
relative to a reference level. The transformation applied is \\I^\* =
I\_{\rm ref} \cdot (I / I\_{\rm ref})^\omega\\, where \\\omega \< 1\\
produces hyperstability and \\\omega \> 1\\ produces hyperdepletion.

## Usage

``` r
hyperstability(object, omega = 1, ref = yearMeans(object))
```

## Arguments

- object:

  An `FLQuant` abundance index to transform.

- omega:

  Numeric scalar; exponent controlling the degree of hyperstability
  (\\\omega \< 1\\) or hyperdepletion (\\\omega \> 1\\). Defaults to 1
  (no transformation).

- ref:

  Reference level of the index; defaults to the year means of `object`.

## Value

An `FLQuant` of the same dimensions as `object` with the
hyperstability/hyperdepletion transformation applied.

## See also

[survey](survey.md), [cpue](cpue.md)

## Author

The FLR Team
