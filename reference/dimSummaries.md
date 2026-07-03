# Summaries by dimension

Methods to compute various summary calculations (sum, mean, variance)
over selected dimensions of objects from any array-based classes (e.g.
`FLQuant`). These methods return an object of the same dimensions as the
input but with length one in the dimension chosen to operate along.

## Usage

``` r
quantSums(x, ...)

yearSums(x, ...)

unitSums(x, ...)

seasonSums(x, ...)

areaSums(x, ...)

iterSums(x, ...)

dimSums(x, ...)

quantMeans(x, ...)

yearMedians(x, ...)

yearMeans(x, ...)

unitMeans(x, ...)

seasonMeans(x, ...)

areaMeans(x, ...)

iterMeans(x, ...)

dimMeans(x, ...)

quantVars(x, ...)

yearVars(x, ...)

unitVars(x, ...)

seasonVars(x, ...)

areaVars(x, ...)

iterVars(x, ...)

dimVars(x, ...)

iterMedians(x, ...)

iterCVs(x, ...)

iterProb(x, ...)

# S4 method for class 'FLQuant'
quantSums(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
yearSums(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
unitSums(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
seasonSums(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
areaSums(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
iterSums(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
quantMeans(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
yearMeans(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
unitMeans(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
seasonMeans(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
areaMeans(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
iterMeans(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
yearMedians(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
iterMedians(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
quantVars(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
yearVars(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
unitVars(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
seasonVars(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
areaVars(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
iterVars(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
iterCVs(x, na.rm = TRUE)

# S4 method for class 'FLQuant'
iterProb(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
yearSums(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
unitSums(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
seasonSums(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
areaSums(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
yearMeans(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
unitMeans(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
seasonMeans(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
areaMeans(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
iterMeans(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
iterMedians(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
quantVars(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
yearVars(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
unitVars(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
seasonVars(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
areaVars(x, na.rm = TRUE)

# S4 method for class 'FLQuantDistr'
iterVars(x, na.rm = TRUE)

# S4 method for class 'FLPar'
iterMeans(x, na.rm = TRUE)

# S4 method for class 'FLPar'
iterMedians(x, na.rm = TRUE)

# S4 method for class 'FLPar'
iterVars(x, na.rm = TRUE)

# S4 method for class 'FLPar'
iterSums(x, na.rm = TRUE)
```

## Arguments

- x:

  An object.

- na.rm:

  Should NAs be removed before calculation? Defaults to TRUE.

## Details

This set of methods computes three different summaries (sum, mean and
variance) of an `FLQuant` object along each of the six dimensions
(quant, year, unit, season, area, or iter). Medians and CVs can also be
computed along the sixth dimension, `iter`.

These methods encapsulate a call to
[`apply`](https://rdrr.io/r/base/apply.html) with the corresponding
dimensions and function: [`mean`](https://rdrr.io/r/base/mean.html),
[`median`](https://rdrr.io/r/stats/median.html),
[`var`](https://rdrr.io/r/stats/cor.html), and
[`sum`](https://rdrr.io/r/base/sum.html), while `iterCVs` are computed
as `sqrt(iterVars) / iterMeans`.

In contrast with R standard behaviour, the sum of a dimension where all
elements are `NA` will be `NA` and not 0. See example below.

Methods working along the iter dimension are also defined for objects of
class `FLPar`.

Methods to operate over the first dimension refer to it as the `quant`
dimension, regardless of the actual name used in the object.

## Generic methods

quantSums(x), quantMeans(x), quantVars(x) yearSums(x), yearMeans(x),
yearVars(x) unitSums(x), unitMeans(x), unitVars(x) seasonSums(x),
seasonMeans(x), seasonVars(x) areaSums(x), areaMeans(x), areaVars(x)
iterMeans(x), iterVars(x), iterMedians(x), iterSums(x) dimSums(x),
dimMeans(x), dimVars(x)

## See also

[FLQuant](FLQuant.md), [sum](https://rdrr.io/r/base/sum.html),
[mean](https://rdrr.io/r/base/mean.html),
[var](https://rdrr.io/r/stats/cor.html)

## Author

The FLR Team

## Examples

``` r

flq <- FLQuant(rnorm(4000), dim=c(5,10,2,2,2,10), quant='age')

quantSums(flq)
#> An object of class "FLQuant"
#> iters:  10 
#> 
#> , , unit = 1, season = 1, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all  0.72691(1.918)  0.79219(1.492)  0.26430(1.167) -0.84921(1.400)
#>      year
#> age   5               6               7               8              
#>   all -0.17113(0.725) -2.15004(2.379)  1.61779(0.548) -0.15631(2.156)
#>      year
#> age   9               10             
#>   all  0.25457(1.920) -1.10483(3.210)
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.38231(1.296)  0.03386(1.783) -0.85239(3.056)  1.00634(1.293)
#>      year
#> age   5               6               7               8              
#>   all -0.52441(3.549)  0.03995(1.291) -1.58016(2.367)  0.59183(2.925)
#>      year
#> age   9               10             
#>   all -0.30601(3.387) -0.21069(2.643)
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all -1.63784(1.596)  0.26667(1.431) -1.31201(1.978) -0.70697(3.233)
#>      year
#> age   5               6               7               8              
#>   all -1.51781(0.854)  1.41103(3.712)  0.13091(3.445)  2.20018(2.022)
#>      year
#> age   9               10             
#>   all  0.70426(1.486)  0.22721(2.353)
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.77804(1.493) -0.00227(1.716)  0.35931(1.811) -0.55798(1.792)
#>      year
#> age   5               6               7               8              
#>   all -1.60882(1.402)  0.89762(1.291)  0.31653(2.718)  1.80951(2.269)
#>      year
#> age   9               10             
#>   all  0.05293(1.379) -0.23511(1.848)
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.01456(1.585) -1.14424(0.626) -0.65703(1.782)  0.58538(3.247)
#>      year
#> age   5               6               7               8              
#>   all -1.02284(1.858) -1.26194(1.885)  1.00073(2.098) -0.34472(2.294)
#>      year
#> age   9               10             
#>   all -2.56992(1.455) -0.48946(1.681)
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all  0.90133(0.970)  0.80953(1.552)  0.77842(1.948)  1.11545(1.136)
#>      year
#> age   5               6               7               8              
#>   all  0.57580(1.929)  0.58828(2.242)  0.67908(1.468)  0.00604(2.533)
#>      year
#> age   9               10             
#>   all  0.89946(1.506) -0.58680(1.697)
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.45336(2.489)  0.49811(1.681)  0.34757(3.660) -1.09210(2.960)
#>      year
#> age   5               6               7               8              
#>   all -0.64799(1.530)  0.44179(1.576) -0.10877(2.856) -1.32152(1.913)
#>      year
#> age   9               10             
#>   all  0.24407(2.573)  0.08380(0.999)
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all  0.75745(1.460) -0.75244(1.683) -0.77135(2.048) -0.93534(2.127)
#>      year
#> age   5               6               7               8              
#>   all  0.22805(2.253) -0.73216(2.507)  0.81590(2.252)  1.55997(2.357)
#>      year
#> age   9               10             
#>   all  1.39646(2.981)  0.59084(2.186)
#> 
#> units:  NA 
quantMeans(flq)
#> An object of class "FLQuant"
#> iters:  10 
#> 
#> , , unit = 1, season = 1, area = 1
#> 
#>      year
#> age   1                2                3                4               
#>   all  0.145382(0.384)  0.158438(0.298)  0.052861(0.233) -0.169842(0.280)
#>      year
#> age   5                6                7                8               
#>   all -0.034226(0.145) -0.430009(0.476)  0.323559(0.110) -0.031262(0.431)
#>      year
#> age   9                10              
#>   all  0.050915(0.384) -0.220965(0.642)
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.076462(0.259)  0.006771(0.357) -0.170477(0.611)  0.201268(0.259)
#>      year
#> age   5                6                7                8               
#>   all -0.104882(0.710)  0.007990(0.258) -0.316033(0.473)  0.118366(0.585)
#>      year
#> age   9                10              
#>   all -0.061203(0.677) -0.042138(0.529)
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.327569(0.319)  0.053334(0.286) -0.262403(0.396) -0.141395(0.647)
#>      year
#> age   5                6                7                8               
#>   all -0.303562(0.171)  0.282206(0.742)  0.026182(0.689)  0.440035(0.404)
#>      year
#> age   9                10              
#>   all  0.140852(0.297)  0.045443(0.471)
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.155607(0.299) -0.000453(0.343)  0.071863(0.362) -0.111596(0.358)
#>      year
#> age   5                6                7                8               
#>   all -0.321763(0.280)  0.179524(0.258)  0.063306(0.544)  0.361902(0.454)
#>      year
#> age   9                10              
#>   all  0.010586(0.276) -0.047021(0.370)
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.002913(0.317) -0.228848(0.125) -0.131406(0.356)  0.117077(0.649)
#>      year
#> age   5                6                7                8               
#>   all -0.204569(0.372) -0.252387(0.377)  0.200146(0.420) -0.068943(0.459)
#>      year
#> age   9                10              
#>   all -0.513983(0.291) -0.097893(0.336)
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>      year
#> age   1                2                3                4               
#>   all  0.180265(0.194)  0.161905(0.310)  0.155685(0.390)  0.223090(0.227)
#>      year
#> age   5                6                7                8               
#>   all  0.115159(0.386)  0.117655(0.448)  0.135816(0.294)  0.001209(0.507)
#>      year
#> age   9                10              
#>   all  0.179891(0.301) -0.117361(0.339)
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.090672(0.498)  0.099623(0.336)  0.069514(0.732) -0.218420(0.592)
#>      year
#> age   5                6                7                8               
#>   all -0.129598(0.306)  0.088358(0.315) -0.021754(0.571) -0.264304(0.383)
#>      year
#> age   9                10              
#>   all  0.048813(0.515)  0.016761(0.200)
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>      year
#> age   1                2                3                4               
#>   all  0.151489(0.292) -0.150488(0.337) -0.154271(0.410) -0.187069(0.425)
#>      year
#> age   5                6                7                8               
#>   all  0.045611(0.451) -0.146432(0.501)  0.163180(0.450)  0.311994(0.471)
#>      year
#> age   9                10              
#>   all  0.279293(0.596)  0.118168(0.437)
#> 
#> units:  NA 
yearSums(flq)
#> An object of class "FLQuant"
#> iters:  10 
#> 
#> , , unit = 1, season = 1, area = 1
#> 
#>    year
#> age 1             
#>   1 -1.65078(2.94)
#>   2 -1.05491(2.60)
#>   3  0.90229(3.93)
#>   4 -0.33139(4.59)
#>   5 -0.09130(2.54)
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>    year
#> age 1             
#>   1 -1.55617(2.96)
#>   2 -0.23762(2.11)
#>   3 -1.37840(2.49)
#>   4 -1.01811(3.13)
#>   5  3.05797(4.11)
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>    year
#> age 1             
#>   1 -0.39701(2.18)
#>   2 -1.40696(2.87)
#>   3 -0.44212(3.83)
#>   4  0.88495(3.87)
#>   5  3.14972(2.46)
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>    year
#> age 1             
#>   1  1.40867(1.53)
#>   2 -1.08081(3.72)
#>   3  1.92607(4.98)
#>   4 -1.02460(1.86)
#>   5 -0.71272(2.73)
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>    year
#> age 1             
#>   1  0.00849(4.07)
#>   2  0.15538(2.87)
#>   3 -2.51771(3.47)
#>   4 -0.37961(3.83)
#>   5 -1.54957(2.40)
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>    year
#> age 1             
#>   1 -1.36556(3.48)
#>   2  1.02992(3.33)
#>   3  0.13643(2.63)
#>   4  1.02258(1.30)
#>   5  1.77911(1.87)
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>    year
#> age 1             
#>   1  0.09846(2.12)
#>   2 -1.46379(3.55)
#>   3 -0.15438(1.42)
#>   4 -0.17823(2.31)
#>   5  0.34867(2.66)
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>    year
#> age 1             
#>   1 -1.46781(1.32)
#>   2  1.73133(1.91)
#>   3  2.34031(3.40)
#>   4  0.77418(3.56)
#>   5 -0.92737(5.00)
#> 
#> units:  NA 
iterMeans(flq)
#> An object of class "FLQuant"
#> , , unit = 1, season = 1, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.199397  0.344760 -0.248606 -0.396944  0.069165 -0.215428  0.756790
#>   2 -0.094059  0.334626  0.420501 -0.324866 -0.296372  0.030724  0.379612
#>   3  0.437694  0.245448  0.066976 -0.099387  0.126952 -0.303695 -0.234742
#>   4 -0.098867  0.273866 -0.154328  0.070841 -0.046107 -0.357717 -0.003343
#>   5  0.316427 -0.166848 -0.153679 -0.003033  0.272707 -0.195319  0.251765
#>    year
#> age 8         9         10       
#>   1  0.177339 -0.451374 -0.659334
#>   2 -0.040305 -0.156478 -0.124669
#>   3 -0.605752  0.462301  0.432782
#>   4 -0.061881  0.775432 -0.510921
#>   5 -0.228401 -0.233151 -0.086262
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.161187  0.064098  0.259621 -0.715189 -0.116724 -0.320908 -0.009771
#>   2  0.520940 -0.196613 -0.311611  0.541041 -0.165898 -0.172172 -0.565408
#>   3 -0.758577  0.003532  0.157556  0.147694 -0.302154 -0.063312 -0.208369
#>   4 -0.016490 -0.340929 -0.362015  0.240755 -0.464093  0.338425 -0.464016
#>   5  0.805602 -0.167488  0.091434  0.617317 -0.026481  0.241995  0.337173
#>    year
#> age 8         9         10       
#>   1  0.344595 -0.168526  0.017317
#>   2  0.549580  0.058619 -0.326854
#>   3  0.389197  0.153848 -0.253999
#>   4 -0.262674 -0.404147 -0.136433
#>   5 -0.050641  0.063375 -0.049001
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.576714 -0.373536 -0.251078  0.333148 -0.066035  0.279832 -0.116134
#>   2 -0.298563 -0.244875  0.139003 -0.302591 -1.063523  0.528923 -0.178579
#>   3 -0.025375  0.606918 -0.329846 -0.779610 -0.070738 -0.307281 -0.026035
#>   4 -0.222283  0.080443  0.227293  0.003586 -0.211277  0.027908 -0.294504
#>   5 -0.243937  0.089547 -0.255441  0.097309  0.389937  0.341735  0.489589
#>    year
#> age 8         9         10       
#>   1  0.352984  0.425534 -0.395619
#>   2  0.269698 -0.028570 -0.457280
#>   3  0.696365 -0.104625 -0.183709
#>   4  0.290428  0.499587  0.562078
#>   5  0.435826 -0.206435  0.424327
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.104616 -0.074565  0.149591  0.376289  0.047132 -0.145179  0.567746
#>   2 -0.484522  0.264028 -0.122115 -0.012777 -0.540651 -0.123843 -0.368087
#>   3  0.467280  0.539826  0.187869 -0.454229 -0.177971 -0.032665  0.351441
#>   4 -0.184988  0.099113  0.108094  0.118877 -0.604099  0.127912  0.272220
#>   5 -0.389716 -0.444377  0.323630  0.206065 -0.130102 -0.026219 -0.367377
#>    year
#> age 8         9         10       
#>   1  0.178608  0.121444  0.107940
#>   2  0.405036 -0.262025 -0.222435
#>   3  0.439053 -0.069489 -0.005832
#>   4  0.024885  0.123160 -0.247385
#>   5  0.130819  0.173265  0.083432
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.058604 -0.091239  0.000645  0.230337 -0.093480  0.213712  0.374325
#>   2  0.338982 -0.692643  0.073077 -0.241033 -0.137528 -0.175946  0.399869
#>   3 -0.106475 -0.093437  0.180245  0.050984 -0.376389 -0.819728 -0.407411
#>   4 -0.077452 -0.282321 -0.117467  0.271382  0.182121  0.027355 -0.018434
#>   5 -0.259276 -0.088857 -0.075623 -0.105678 -0.259877 -0.206988  0.282935
#>    year
#> age 8         9         10       
#>   1 -0.109575 -0.412468 -0.242035
#>   2  0.409190 -0.714665  0.013479
#>   3 -0.239394 -0.405850 -0.132768
#>   4  0.359685 -0.443776 -0.300697
#>   5 -0.314457 -0.375734  0.156932
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.026071  0.103945  0.331129  0.241738 -0.303993  0.076859 -0.302723
#>   2  0.132491  0.299681  0.335775 -0.134465 -0.298209 -0.106552  0.392692
#>   3 -0.424959 -0.023317  0.163902  0.009407  0.792677 -0.123211  0.537351
#>   4  0.089540  0.229452  0.050022  0.145177 -0.082810  0.073842 -0.137022
#>   5  0.686532 -0.056481  0.199634  0.422044 -0.298540  0.145268  0.262348
#>    year
#> age 8         9         10       
#>   1  0.400325 -0.684283 -0.603186
#>   2 -0.126572  0.260068  0.063762
#>   3  0.425957 -0.263679  0.161176
#>   4 -0.342997  0.443389  0.231993
#>   5  0.051308  0.705858 -0.013590
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.414420  0.269504 -0.056283  0.034171 -0.858951  0.020856 -0.005524
#>   2 -0.337621 -0.007379  0.598281 -0.257099  0.107654 -0.111369 -0.485825
#>   3  0.032981 -0.245664 -0.204468  0.344686 -0.536070 -0.041523 -0.016296
#>   4 -0.121332  0.012404 -0.348672 -0.473929  0.542434  0.197094 -0.043863
#>   5  0.287098  0.318446 -0.217897 -0.538417  0.419164  0.166710  0.317009
#>    year
#> age 8         9         10       
#>   1 -0.267172  0.412116  0.147309
#>   2 -0.453444  0.115104 -0.141699
#>   3 -0.218243  0.030521  0.060520
#>   4 -0.012219 -0.197048  0.187045
#>   5 -0.137699 -0.289231 -0.166311
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.294917 -0.378188 -0.190439 -0.225714 -0.144508 -0.547615  0.025542
#>   2  0.690690  0.129141  0.206383  0.222203  0.619937 -0.559777 -0.047781
#>   3 -0.001328 -0.147361 -0.053811  0.246123  0.166434  0.409121 -0.121034
#>   4  0.064980  0.193907 -0.318792 -0.422570 -0.028207  0.344808  0.052656
#>   5  0.130171  0.060895 -0.512405 -0.693082 -0.080637 -0.572283  0.191731
#>    year
#> age 8         9         10       
#>   1  0.307477 -0.244550 -0.102971
#>   2  0.463654  0.277837  0.086710
#>   3  0.649053  0.290859  0.079030
#>   4  0.038505  0.044410 -0.186043
#>   5 -0.276475  0.420755 -0.214412
#> 
#> units:  NA 
dim(quantSums(flq))
#> [1]  1 10  2  2  2 10

# NA dims stay as NA when summed along
x <- FLQuant(c(NA, NA, NA, rnorm(6)), dim=c(3, 3))
quantSums(x)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1     2     3    
#>   all    NA -2.22  1.81
#> 
#> units:  NA 
# although in fact a sum of no elements (as na.rm=TRUE) is zero
apply(x, 2:6, sum, na.rm=TRUE)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1     2     3    
#>   all  0.00 -2.22  1.81
#> 
#> units:  NA 
```
