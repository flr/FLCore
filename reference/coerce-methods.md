# Convert Objects Between Classes

Objects of various **FLCore** classes can be converted into other
classes, both basic R ones, like `data.frame`, and others defined in the
package. For the specifics of the precise calculations carried out for
each pair of classes, see below.

## Arguments

- object:

  Object to be converted.

- Class:

  Name of the class to convert the object to, `character`.

## Value

An object of the requested class.

## FLArray to data.frame

The six dimensions of an `FLArray` are converted into seven columns,
named `quant` (or any other name given to the first dimension in the
object), `year`, `unit`, `season`, `area`, `iter` and `data`. The last
one contains the actual numbers stored in the array. `units` are stored
as an attribute to the `data.frame`. The `year` and `data` columns are
of type `numeric`, while all others are `factor`.

## FLPar to data.frame

The two or more dimensions of an *FLPar* objects are converted into
three or more columns. For a 2D objects, they are named *params*, *iter*
and *data*. The last one contains the actual numbers stored in the
array, in a column type `numeric`, while all others are `factor`.

## See also

base::as, base::coerce

## Author

The FLR Team

## Examples

``` r
# from FLQuant to data.frame
as(FLQuant(rnorm(100), dim=c(5, 20)), "data.frame")
#>     quant year   unit season   area iter         data
#> 1       1    1 unique    all unique    1 -0.219024446
#> 2       2    1 unique    all unique    1  0.008369587
#> 3       3    1 unique    all unique    1 -0.290741234
#> 4       4    1 unique    all unique    1 -0.215558718
#> 5       5    1 unique    all unique    1  0.621358173
#> 6       1    2 unique    all unique    1 -0.913926411
#> 7       2    2 unique    all unique    1  0.313734983
#> 8       3    2 unique    all unique    1  0.879905108
#> 9       4    2 unique    all unique    1 -0.351130640
#> 10      5    2 unique    all unique    1 -1.824247200
#> 11      1    3 unique    all unique    1 -1.279339877
#> 12      2    3 unique    all unique    1 -1.592609993
#> 13      3    3 unique    all unique    1 -1.472094029
#> 14      4    3 unique    all unique    1  0.868931876
#> 15      5    3 unique    all unique    1  0.407377006
#> 16      1    4 unique    all unique    1 -0.949259933
#> 17      2    4 unique    all unique    1 -0.086388772
#> 18      3    4 unique    all unique    1 -0.811054239
#> 19      4    4 unique    all unique    1  0.221324492
#> 20      5    4 unique    all unique    1 -1.023152353
#> 21      1    5 unique    all unique    1 -0.004465181
#> 22      2    5 unique    all unique    1  0.880233578
#> 23      3    5 unique    all unique    1 -1.982349310
#> 24      4    5 unique    all unique    1 -0.585358329
#> 25      5    5 unique    all unique    1 -1.845920215
#> 26      1    6 unique    all unique    1 -0.347057895
#> 27      2    6 unique    all unique    1  1.689775598
#> 28      3    6 unique    all unique    1  0.379050795
#> 29      4    6 unique    all unique    1 -0.459959239
#> 30      5    6 unique    all unique    1  0.767678135
#> 31      1    7 unique    all unique    1  1.707361650
#> 32      2    7 unique    all unique    1  1.131363415
#> 33      3    7 unique    all unique    1  0.890468643
#> 34      4    7 unique    all unique    1  1.881834283
#> 35      5    7 unique    all unique    1 -0.642540707
#> 36      1    8 unique    all unique    1  0.837664328
#> 37      2    8 unique    all unique    1 -0.567759708
#> 38      3    8 unique    all unique    1  0.535235735
#> 39      4    8 unique    all unique    1 -0.530982732
#> 40      5    8 unique    all unique    1  0.729320408
#> 41      1    9 unique    all unique    1 -0.552114798
#> 42      2    9 unique    all unique    1  0.031801214
#> 43      3    9 unique    all unique    1 -1.891338980
#> 44      4    9 unique    all unique    1 -0.056759478
#> 45      5    9 unique    all unique    1  0.865373596
#> 46      1   10 unique    all unique    1  0.220081461
#> 47      2   10 unique    all unique    1 -0.595956703
#> 48      3   10 unique    all unique    1 -0.122960377
#> 49      4   10 unique    all unique    1  0.078194024
#> 50      5   10 unique    all unique    1 -0.963582785
#> 51      1   11 unique    all unique    1 -1.253456258
#> 52      2   11 unique    all unique    1  0.512253919
#> 53      3   11 unique    all unique    1 -0.211692262
#> 54      4   11 unique    all unique    1 -0.745340417
#> 55      5   11 unique    all unique    1 -1.193234702
#> 56      1   12 unique    all unique    1  2.055037250
#> 57      2   12 unique    all unique    1  1.632423556
#> 58      3   12 unique    all unique    1 -0.532611267
#> 59      4   12 unique    all unique    1  0.023402388
#> 60      5   12 unique    all unique    1 -0.948465802
#> 61      1   13 unique    all unique    1  0.153762418
#> 62      2   13 unique    all unique    1 -1.588615779
#> 63      3   13 unique    all unique    1  0.251863348
#> 64      4   13 unique    all unique    1 -0.653536686
#> 65      5   13 unique    all unique    1 -0.792873632
#> 66      1   14 unique    all unique    1  0.272929912
#> 67      2   14 unique    all unique    1  0.712862033
#> 68      3   14 unique    all unique    1  0.282975337
#> 69      4   14 unique    all unique    1  0.363469458
#> 70      5   14 unique    all unique    1  0.060149472
#> 71      1   15 unique    all unique    1 -0.816201990
#> 72      2   15 unique    all unique    1 -0.527232240
#> 73      3   15 unique    all unique    1 -0.612901685
#> 74      4   15 unique    all unique    1 -0.939256421
#> 75      5   15 unique    all unique    1  0.105017700
#> 76      1   16 unique    all unique    1 -1.415856351
#> 77      2   16 unique    all unique    1 -0.072692180
#> 78      3   16 unique    all unique    1  0.731925793
#> 79      4   16 unique    all unique    1 -0.341445412
#> 80      5   16 unique    all unique    1 -1.241268232
#> 81      1   17 unique    all unique    1  1.512478245
#> 82      2   17 unique    all unique    1 -0.822276280
#> 83      3   17 unique    all unique    1 -0.447570597
#> 84      4   17 unique    all unique    1 -1.450567784
#> 85      5   17 unique    all unique    1 -1.072681969
#> 86      1   18 unique    all unique    1  1.500963740
#> 87      2   18 unique    all unique    1  1.905968572
#> 88      3   18 unique    all unique    1  0.598306443
#> 89      4   18 unique    all unique    1  1.297896404
#> 90      5   18 unique    all unique    1 -0.337073824
#> 91      1   19 unique    all unique    1  0.577167439
#> 92      2   19 unique    all unique    1 -2.097895386
#> 93      3   19 unique    all unique    1  0.972224975
#> 94      4   19 unique    all unique    1  1.256833314
#> 95      5   19 unique    all unique    1 -0.939499040
#> 96      1   20 unique    all unique    1  0.156410534
#> 97      2   20 unique    all unique    1 -1.181842236
#> 98      3   20 unique    all unique    1  1.518654052
#> 99      4   20 unique    all unique    1  0.673414648
#> 100     5   20 unique    all unique    1 -0.018161674
# from FLPar to data.frame
as(FLPar(phi=rnorm(10), rho=rlnorm(10)), "data.frame")
#>    params iter        data
#> 1     phi    1  0.11392236
#> 2     rho    1  0.75363221
#> 3     phi    2 -0.01718242
#> 4     rho    2  4.68142851
#> 5     phi    3 -1.31325614
#> 6     rho    3  1.81229495
#> 7     phi    4  1.07562806
#> 8     rho    4 10.55155900
#> 9     phi    5 -0.54081333
#> 10    rho    5  0.24657770
#> 11    phi    6  0.29566743
#> 12    rho    6  0.69506810
#> 13    phi    7  0.15611629
#> 14    rho    7  0.59256155
#> 15    phi    8  1.03588096
#> 16    rho    8  2.86148719
#> 17    phi    9 -0.64455888
#> 18    rho    9  0.68754551
#> 19    phi   10 -1.06708187
#> 20    rho   10  2.00229496
```
