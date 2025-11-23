<div id="main" class="col-md-9" role="main">

# Extend a FLR object along the year dimension and set future assumed values

<div class="ref-description section level2">

Objects to be projected into the future are extended until an end year,
and the values of certain quantities, usually assume constant, are set
following different mechanisms.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
fwdWindow(x, y, ...)

# S4 method for class 'FLStock,missing'
fwdWindow(
  x,
  end = dims(x)$maxyear,
  nsq = 3,
  fun = c("mean", "geomean", "sample"),
  years = list(wt = nsq, mat = nsq, m = nsq, spwn = nsq, discards.ratio = nsq, catch.sel
    = nsq)
)
```

</div>

</div>

<div class="section level2">

## Arguments

-   x:

    The FLR object to extend.

-   y:

    A second object from which information is taken.

</div>

<div class="section level2">

## Value

An object of the same class as 'x'.

</div>

<div class="section level2">

## Details

For 'FLStock'

</div>

<div class="section level2">

## See also

<div class="dont-index">

`window()`

</div>

</div>

<div class="section level2">

## Author

The FLR Team.

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
data(ple4)
# Use mean of last three years and extend until 2020
fut <- fwdWindow(ple4, end=2020)
# Check values on catch.wt
catch.wt(fut)[, ac(2015:2020)]
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  2015   2016   2017   2018   2019   2020  
#>   1  0.0260 0.0480 0.0510 0.0417 0.0417 0.0417
#>   2  0.0803 0.0838 0.0859 0.0833 0.0833 0.0833
#>   3  0.1618 0.1574 0.1586 0.1592 0.1592 0.1592
#>   4  0.2579 0.2430 0.2184 0.2398 0.2398 0.2398
#>   5  0.3256 0.2989 0.3144 0.3130 0.3130 0.3130
#>   6  0.3937 0.3525 0.3863 0.3772 0.3772 0.3772
#>   7  0.4613 0.4223 0.4377 0.4403 0.4403 0.4403
#>   8  0.4808 0.4652 0.5315 0.4940 0.4940 0.4940
#>   9  0.5820 0.5560 0.6420 0.5933 0.5933 0.5933
#>   10 0.5999 0.6839 0.7349 0.6729 0.6729 0.6729
#> 
#> units:  kg 
# Use mean of the 2010:2015 period
fut <- fwdWindow(ple4, end=2020, years=2010:2015)
# Use last three years mean, but last five for 'wt'
fut <- fwdWindow(ple4, end=2020, nsq=3, years=list(wt=5))
stock.wt(fut)[, ac(2013:2020)]
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  2013   2014   2015   2016   2017   2018   2019   2020  
#>   1  0.0430 0.0480 0.0240 0.0300 0.0320 0.0354 0.0354 0.0354
#>   2  0.1070 0.1040 0.0650 0.0660 0.0690 0.0822 0.0822 0.0822
#>   3  0.1530 0.1580 0.1200 0.1170 0.1320 0.1360 0.1360 0.1360
#>   4  0.2080 0.2020 0.2070 0.1980 0.1810 0.1992 0.1992 0.1992
#>   5  0.3200 0.3120 0.2790 0.2600 0.2700 0.2882 0.2882 0.2882
#>   6  0.3540 0.3800 0.3230 0.3290 0.3330 0.3438 0.3438 0.3438
#>   7  0.4340 0.4390 0.3790 0.3800 0.3590 0.3982 0.3982 0.3982
#>   8  0.4930 0.4840 0.4350 0.4340 0.4580 0.4608 0.4608 0.4608
#>   9  0.6620 0.4580 0.4650 0.4790 0.4760 0.5080 0.5080 0.5080
#>   10 0.4680 0.6150 0.4570 0.5140 0.5570 0.5222 0.5222 0.5222
#> 
#> units:  kg 
catch.sel(fut)[, ac(2013:2020)]
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  2013   2014   2015   2016   2017   2018   2019   2020  
#>   1  0.3716 0.5746 0.6742 0.4856 0.2688 0.4725 0.4725 0.4725
#>   2  0.7276 0.8177 0.8487 0.7754 0.6593 0.7597 0.7597 0.7597
#>   3  1.0000 1.0000 0.9979 1.0000 1.0000 1.0000 1.0000 1.0000
#>   4  0.9212 0.9743 1.0000 0.9668 0.9014 0.9557 0.9557 0.9557
#>   5  0.7700 0.7773 0.7803 0.7756 0.7633 0.7734 0.7734 0.7734
#>   6  0.5536 0.5474 0.5461 0.5561 0.5700 0.5580 0.5580 0.5580
#>   7  0.3180 0.3606 0.3844 0.3634 0.3208 0.3558 0.3558 0.3558
#>   8  0.1871 0.2180 0.2305 0.2040 0.1635 0.1988 0.1988 0.1988
#>   9  0.1214 0.1237 0.1178 0.1019 0.0834 0.1007 0.1007 0.1007
#>   10 0.1214 0.1237 0.1178 0.1019 0.0834 0.1007 0.1007 0.1007
#> 
#> units:  NA 
# Resample from last years for 'wt'
fut <- fwdWindow(ple4, end=2020, nsq=3, fun=c(wt='sample'))
# Years to resample can be different for 'catch.sel'
fut <- fwdWindow(ple4, end=2020, nsq=3,
  fun=c(wt='sample', catch.sel='sample'), years=c(wt=10, catch.sel=5))
# 'wt' slot has been resampled,
stock.wt(fut)[, ac(2015:2020)]
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  2015  2016  2017  2018  2019  2020 
#>   1  0.024 0.030 0.032 0.052 0.048 0.052
#>   2  0.065 0.066 0.069 0.114 0.104 0.114
#>   3  0.120 0.117 0.132 0.194 0.158 0.194
#>   4  0.207 0.198 0.181 0.344 0.202 0.344
#>   5  0.279 0.260 0.270 0.373 0.312 0.373
#>   6  0.323 0.329 0.333 0.412 0.380 0.412
#>   7  0.379 0.380 0.359 0.472 0.439 0.472
#>   8  0.435 0.434 0.458 0.540 0.484 0.540
#>   9  0.465 0.479 0.476 0.565 0.458 0.565
#>   10 0.457 0.514 0.557 0.576 0.615 0.576
#> 
#> units:  kg 
# while others have used a 3 year average
catch.sel(fut)[, ac(2015:2020)]
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>     year
#> age  2015   2016   2017   2018   2019   2020  
#>   1  0.6742 0.4856 0.2688 0.6742 0.6742 0.2688
#>   2  0.8487 0.7754 0.6593 0.8487 0.8487 0.6593
#>   3  0.9979 1.0000 1.0000 0.9979 0.9979 1.0000
#>   4  1.0000 0.9668 0.9014 1.0000 1.0000 0.9014
#>   5  0.7803 0.7756 0.7633 0.7803 0.7803 0.7633
#>   6  0.5461 0.5561 0.5700 0.5461 0.5461 0.5700
#>   7  0.3844 0.3634 0.3208 0.3844 0.3844 0.3208
#>   8  0.2305 0.2040 0.1635 0.2305 0.2305 0.1635
#>   9  0.1178 0.1019 0.0834 0.1178 0.1178 0.0834
#>   10 0.1178 0.1019 0.0834 0.1178 0.1178 0.0834
#> 
#> units:  NA 
```

</div>

</div>

</div>
