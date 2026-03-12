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
#> 1       1    1 unique    all unique    1  0.725298945
#> 2       2    1 unique    all unique    1  0.195409250
#> 3       3    1 unique    all unique    1 -0.413650614
#> 4       4    1 unique    all unique    1 -1.328259249
#> 5       5    1 unique    all unique    1  1.730665919
#> 6       1    2 unique    all unique    1  0.187037560
#> 7       2    2 unique    all unique    1 -0.935728727
#> 8       3    2 unique    all unique    1  0.083062326
#> 9       4    2 unique    all unique    1  0.160518049
#> 10      5    2 unique    all unique    1  0.202105749
#> 11      1    3 unique    all unique    1 -1.995176929
#> 12      2    3 unique    all unique    1 -0.227655067
#> 13      3    3 unique    all unique    1 -0.010629297
#> 14      4    3 unique    all unique    1  0.576946108
#> 15      5    3 unique    all unique    1 -0.309330062
#> 16      1    4 unique    all unique    1 -0.361320050
#> 17      2    4 unique    all unique    1 -1.178973937
#> 18      3    4 unique    all unique    1 -0.351561307
#> 19      4    4 unique    all unique    1  0.682996726
#> 20      5    4 unique    all unique    1  0.396960455
#> 21      1    5 unique    all unique    1  1.103677767
#> 22      2    5 unique    all unique    1  0.283071115
#> 23      3    5 unique    all unique    1 -0.119032853
#> 24      4    5 unique    all unique    1 -0.850775902
#> 25      5    5 unique    all unique    1 -0.004881436
#> 26      1    6 unique    all unique    1 -0.555004527
#> 27      2    6 unique    all unique    1 -0.005464035
#> 28      3    6 unique    all unique    1  0.596582274
#> 29      4    6 unique    all unique    1  0.930140787
#> 30      5    6 unique    all unique    1 -1.545631788
#> 31      1    7 unique    all unique    1 -0.393499692
#> 32      2    7 unique    all unique    1  1.127952598
#> 33      3    7 unique    all unique    1 -0.426971110
#> 34      4    7 unique    all unique    1  0.945087699
#> 35      5    7 unique    all unique    1 -1.527816578
#> 36      1    8 unique    all unique    1  0.359213899
#> 37      2    8 unique    all unique    1 -0.857820367
#> 38      3    8 unique    all unique    1  0.810534835
#> 39      4    8 unique    all unique    1 -0.219024446
#> 40      5    8 unique    all unique    1  0.008369587
#> 41      1    9 unique    all unique    1 -0.290741234
#> 42      2    9 unique    all unique    1 -0.215558718
#> 43      3    9 unique    all unique    1  0.621358173
#> 44      4    9 unique    all unique    1 -0.913926411
#> 45      5    9 unique    all unique    1  0.313734983
#> 46      1   10 unique    all unique    1  0.879905108
#> 47      2   10 unique    all unique    1 -0.351130640
#> 48      3   10 unique    all unique    1 -1.824247200
#> 49      4   10 unique    all unique    1 -1.279339877
#> 50      5   10 unique    all unique    1 -1.592609993
#> 51      1   11 unique    all unique    1 -1.472094029
#> 52      2   11 unique    all unique    1  0.868931876
#> 53      3   11 unique    all unique    1  0.407377006
#> 54      4   11 unique    all unique    1 -0.949259933
#> 55      5   11 unique    all unique    1 -0.086388772
#> 56      1   12 unique    all unique    1 -0.811054239
#> 57      2   12 unique    all unique    1  0.221324492
#> 58      3   12 unique    all unique    1 -1.023152353
#> 59      4   12 unique    all unique    1 -0.004465181
#> 60      5   12 unique    all unique    1  0.880233578
#> 61      1   13 unique    all unique    1 -1.982349310
#> 62      2   13 unique    all unique    1 -0.585358329
#> 63      3   13 unique    all unique    1 -1.845920215
#> 64      4   13 unique    all unique    1 -0.347057895
#> 65      5   13 unique    all unique    1  1.689775598
#> 66      1   14 unique    all unique    1  0.379050795
#> 67      2   14 unique    all unique    1 -0.459959239
#> 68      3   14 unique    all unique    1  0.767678135
#> 69      4   14 unique    all unique    1  1.707361650
#> 70      5   14 unique    all unique    1  1.131363415
#> 71      1   15 unique    all unique    1  0.890468643
#> 72      2   15 unique    all unique    1  1.881834283
#> 73      3   15 unique    all unique    1 -0.642540707
#> 74      4   15 unique    all unique    1  0.837664328
#> 75      5   15 unique    all unique    1 -0.567759708
#> 76      1   16 unique    all unique    1  0.535235735
#> 77      2   16 unique    all unique    1 -0.530982732
#> 78      3   16 unique    all unique    1  0.729320408
#> 79      4   16 unique    all unique    1 -0.552114798
#> 80      5   16 unique    all unique    1  0.031801214
#> 81      1   17 unique    all unique    1 -1.891338980
#> 82      2   17 unique    all unique    1 -0.056759478
#> 83      3   17 unique    all unique    1  0.865373596
#> 84      4   17 unique    all unique    1  0.220081461
#> 85      5   17 unique    all unique    1 -0.595956703
#> 86      1   18 unique    all unique    1 -0.122960377
#> 87      2   18 unique    all unique    1  0.078194024
#> 88      3   18 unique    all unique    1 -0.963582785
#> 89      4   18 unique    all unique    1 -1.253456258
#> 90      5   18 unique    all unique    1  0.512253919
#> 91      1   19 unique    all unique    1 -0.211692262
#> 92      2   19 unique    all unique    1 -0.745340417
#> 93      3   19 unique    all unique    1 -1.193234702
#> 94      4   19 unique    all unique    1  2.055037250
#> 95      5   19 unique    all unique    1  1.632423556
#> 96      1   20 unique    all unique    1 -0.532611267
#> 97      2   20 unique    all unique    1  0.023402388
#> 98      3   20 unique    all unique    1 -0.948465802
#> 99      4   20 unique    all unique    1  0.153762418
#> 100     5   20 unique    all unique    1 -1.588615779
# from FLPar to data.frame
as(FLPar(phi=rnorm(10), rho=rlnorm(10)), "data.frame")
#>    params iter        data
#> 1     phi    1  0.25186335
#> 2     rho    1  0.54177652
#> 3     phi    2 -0.65353669
#> 4     rho    2  0.39091841
#> 5     phi    3 -0.79287363
#> 6     rho    3  1.11073027
#> 7     phi    4  0.27292991
#> 8     rho    4  0.24271767
#> 9     phi    5  0.71286203
#> 10    rho    5  0.92988702
#> 11    phi    6  0.28297534
#> 12    rho    6  2.07908063
#> 13    phi    7  0.36346946
#> 14    rho    7  0.71074226
#> 15    phi    8  0.06014947
#> 16    rho    8  0.28901744
#> 17    phi    9 -0.81620199
#> 18    rho    9  4.53796306
#> 19    phi   10 -0.52723224
#> 20    rho   10  0.43943025
```
