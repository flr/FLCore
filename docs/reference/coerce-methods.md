<div id="main" class="col-md-9" role="main">

# Convert Objects Between Classes

<div class="ref-description section level2">

Objects of various **FLCore** classes can be converted into other
classes, both basic R ones, like `data.frame`, and others defined in the
package. For the specifics of the precise calculations carried out for
each pair of classes, see below.

</div>

<div class="section level2">

## Arguments

-   object:

    Object to be converted.

-   Class:

    Name of the class to convert the object to, `character`.

</div>

<div class="section level2">

## Value

An object of the requested class.

</div>

<div class="section level2">

## FLArray to data.frame

The six dimensions of an `FLArray` are converted into seven columns,
named `quant` (or any other name given to the first dimension in the
object), `year`, `unit`, `season`, `area`, `iter` and `data`. The last
one contains the actual numbers stored in the array. `units` are stored
as an attribute to the `data.frame`. The `year` and `data` columns are
of type `numeric`, while all others are `factor`.

</div>

<div class="section level2">

## FLPar to data.frame

The two or more dimensions of an *FLPar* objects are converted into
three or more columns. For a 2D objects, they are named *params*, *iter*
and *data*. The last one contains the actual numbers stored in the
array, in a column type `numeric`, while all others are `factor`.

</div>

<div class="section level2">

## See also

<div class="dont-index">

base::as, base::coerce

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
# from FLQuant to data.frame
as(FLQuant(rnorm(100), dim=c(5, 20)), "data.frame")
#>     quant year   unit season   area iter        data
#> 1       1    1 unique    all unique    1  1.23387018
#> 2       2    1 unique    all unique    1  0.38769108
#> 3       3    1 unique    all unique    1 -0.16425606
#> 4       4    1 unique    all unique    1 -0.41694062
#> 5       5    1 unique    all unique    1 -0.01955970
#> 6       1    2 unique    all unique    1 -1.28679411
#> 7       2    2 unique    all unique    1 -1.06172725
#> 8       3    2 unique    all unique    1 -0.65346943
#> 9       4    2 unique    all unique    1  0.54605449
#> 10      5    2 unique    all unique    1 -0.55388324
#> 11      1    3 unique    all unique    1  1.08120000
#> 12      2    3 unique    all unique    1  0.61657147
#> 13      3    3 unique    all unique    1  0.10733408
#> 14      4    3 unique    all unique    1 -0.00991494
#> 15      5    3 unique    all unique    1 -0.77790812
#> 16      1    4 unique    all unique    1  1.16752702
#> 17      2    4 unique    all unique    1  0.43972735
#> 18      3    4 unique    all unique    1 -0.79019806
#> 19      4    4 unique    all unique    1 -1.23396101
#> 20      5    4 unique    all unique    1  0.43384015
#> 21      1    5 unique    all unique    1  0.28184582
#> 22      2    5 unique    all unique    1 -1.73771157
#> 23      3    5 unique    all unique    1 -0.85059636
#> 24      4    5 unique    all unique    1  1.86252316
#> 25      5    5 unique    all unique    1  1.23862928
#> 26      1    6 unique    all unique    1 -0.59566744
#> 27      2    6 unique    all unique    1  0.91076876
#> 28      3    6 unique    all unique    1 -0.26576998
#> 29      4    6 unique    all unique    1  0.31774589
#> 30      5    6 unique    all unique    1  0.37823141
#> 31      1    7 unique    all unique    1 -0.90150371
#> 32      2    7 unique    all unique    1  1.21432212
#> 33      3    7 unique    all unique    1 -0.22544166
#> 34      4    7 unique    all unique    1 -0.21746329
#> 35      5    7 unique    all unique    1  1.04560399
#> 36      1    8 unique    all unique    1  1.79337756
#> 37      2    8 unique    all unique    1 -0.10441185
#> 38      3    8 unique    all unique    1  0.78948099
#> 39      4    8 unique    all unique    1 -2.07292184
#> 40      5    8 unique    all unique    1  2.21463109
#> 41      1    9 unique    all unique    1  1.57844773
#> 42      2    9 unique    all unique    1  1.22404338
#> 43      3    9 unique    all unique    1 -0.04916270
#> 44      4    9 unique    all unique    1  0.34662763
#> 45      5    9 unique    all unique    1  2.19509878
#> 46      1   10 unique    all unique    1  0.57898798
#> 47      2   10 unique    all unique    1 -0.69747658
#> 48      3   10 unique    all unique    1  0.44808265
#> 49      4   10 unique    all unique    1  0.33083346
#> 50      5   10 unique    all unique    1 -0.99741189
#> 51      1   11 unique    all unique    1 -0.14125862
#> 52      2   11 unique    all unique    1 -0.90833732
#> 53      3   11 unique    all unique    1  0.55064754
#> 54      4   11 unique    all unique    1 -0.80809562
#> 55      5   11 unique    all unique    1 -1.83893557
#> 56      1   12 unique    all unique    1  0.90553241
#> 57      2   12 unique    all unique    1 -1.45875228
#> 58      3   12 unique    all unique    1 -0.80164698
#> 59      4   12 unique    all unique    1 -0.91078482
#> 60      5   12 unique    all unique    1 -1.40536731
#> 61      1   13 unique    all unique    1  2.19468463
#> 62      2   13 unique    all unique    1 -1.13869430
#> 63      3   13 unique    all unique    1  0.18226005
#> 64      4   13 unique    all unique    1 -0.32061023
#> 65      5   13 unique    all unique    1  0.40637906
#> 66      1   14 unique    all unique    1 -0.62286968
#> 67      2   14 unique    all unique    1 -0.73225294
#> 68      3   14 unique    all unique    1  0.25405712
#> 69      4   14 unique    all unique    1  0.15130720
#> 70      5   14 unique    all unique    1 -0.44646945
#> 71      1   15 unique    all unique    1  0.26325311
#> 72      2   15 unique    all unique    1  1.70908398
#> 73      3   15 unique    all unique    1  1.29245373
#> 74      4   15 unique    all unique    1  0.01181277
#> 75      5   15 unique    all unique    1  0.65081795
#> 76      1   16 unique    all unique    1 -0.93761490
#> 77      2   16 unique    all unique    1 -0.23639677
#> 78      3   16 unique    all unique    1 -1.83714775
#> 79      4   16 unique    all unique    1  0.22549552
#> 80      5   16 unique    all unique    1  0.02823558
#> 81      1   17 unique    all unique    1  0.97174543
#> 82      2   17 unique    all unique    1 -0.14679718
#> 83      3   17 unique    all unique    1 -0.53589126
#> 84      4   17 unique    all unique    1  1.04320648
#> 85      5   17 unique    all unique    1 -0.17645298
#> 86      1   18 unique    all unique    1  1.71164825
#> 87      2   18 unique    all unique    1  0.32105509
#> 88      3   18 unique    all unique    1  1.66660161
#> 89      4   18 unique    all unique    1  0.04829806
#> 90      5   18 unique    all unique    1 -2.03667157
#> 91      1   19 unique    all unique    1 -1.40043614
#> 92      2   19 unique    all unique    1 -0.37089381
#> 93      3   19 unique    all unique    1 -1.27339328
#> 94      4   19 unique    all unique    1 -0.37045727
#> 95      5   19 unique    all unique    1 -0.92801062
#> 96      1   20 unique    all unique    1  0.12432197
#> 97      2   20 unique    all unique    1  0.10874821
#> 98      3   20 unique    all unique    1  0.06227053
#> 99      4   20 unique    all unique    1 -0.18010410
#> 100     5   20 unique    all unique    1  0.31308589
# from FLPar to data.frame
as(FLPar(phi=rnorm(10), rho=rlnorm(10)), "data.frame")
#>    params iter       data
#> 1     phi    1  1.1799555
#> 2     rho    1  1.4867351
#> 3     phi    2  0.5610449
#> 4     rho    2  0.1184486
#> 5     phi    3  1.9456456
#> 6     rho    3  0.8402111
#> 7     phi    4 -1.1662985
#> 8     rho    4  3.0798886
#> 9     phi    5 -0.3161725
#> 10    rho    5  0.5380848
#> 11    phi    6 -0.0872654
#> 12    rho    6  6.6232743
#> 13    phi    7  0.7547837
#> 14    rho    7  2.8198217
#> 15    phi    8 -0.6425026
#> 16    rho    8  1.5865419
#> 17    phi    9 -0.2653612
#> 18    rho    9 12.4952443
#> 19    phi   10  0.7257943
#> 20    rho   10  2.4945660
```

</div>

</div>

</div>
