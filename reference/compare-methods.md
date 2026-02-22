# A method for comparing FLR objects

Comparisons of complete objects of FLR classes can be carried out and a
report table is generated to better identify differences. Comparisons do
not substitute but complement those provided by R's all.equal and
identical.

## Usage

``` r
compare(result, target, ...)
```

## Arguments

- result:

  First element in comparison, result of method or operation.

- target:

  Second element, desired output.

## Value

A table of comparisons, of class data.frame.

## Author

Iago Mosqueira (WMR)
