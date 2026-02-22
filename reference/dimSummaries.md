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
#>   all -0.12328(0.833)  0.75745(1.460) -0.75244(1.414) -1.21170(1.958)
#>      year
#> age   5               6               7               8              
#>   all -0.93534(2.127)  0.22805(1.406) -0.69218(3.780)  0.67084(2.252)
#>      year
#> age   9               10             
#>   all  1.55997(2.749)  0.72154(3.081)
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all  0.59084(2.186)  0.72691(1.918)  0.79219(1.492)  0.26430(1.167)
#>      year
#> age   5               6               7               8              
#>   all -0.84921(1.400) -0.17113(0.725) -2.15004(2.379)  1.61779(0.548)
#>      year
#> age   9               10             
#>   all -0.15631(2.156)  0.25457(1.920)
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all -1.10483(3.210) -0.38231(1.296)  0.03386(1.783) -0.85239(3.056)
#>      year
#> age   5               6               7               8              
#>   all  1.00634(1.293) -0.52441(3.549)  0.03995(1.291) -1.58016(2.367)
#>      year
#> age   9               10             
#>   all  0.59183(2.925) -0.30601(3.387)
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.21069(2.643) -1.63784(1.596)  0.26667(1.431) -1.31201(1.978)
#>      year
#> age   5               6               7               8              
#>   all -0.70697(3.233) -1.51781(0.854)  1.41103(3.712)  0.13091(3.445)
#>      year
#> age   9               10             
#>   all  2.20018(2.022)  0.70426(1.486)
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all  0.22721(2.353) -0.77804(1.493) -0.00227(1.716)  0.35931(1.811)
#>      year
#> age   5               6               7               8              
#>   all -0.55798(1.792) -1.60882(1.402)  0.89762(1.291)  0.31653(2.718)
#>      year
#> age   9               10             
#>   all  1.80951(2.269)  0.05293(1.379)
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.23511(1.848) -0.01456(1.585) -1.14424(0.626) -0.65703(1.782)
#>      year
#> age   5               6               7               8              
#>   all  0.58538(3.247) -1.02284(1.858) -1.26194(1.885)  1.00073(2.098)
#>      year
#> age   9               10             
#>   all -0.34472(2.294) -2.56992(1.455)
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.48946(1.681)  0.90133(0.970)  0.80953(1.552)  0.77842(1.948)
#>      year
#> age   5               6               7               8              
#>   all  1.11545(1.136)  0.57580(1.929)  0.58828(2.242)  0.67908(1.468)
#>      year
#> age   9               10             
#>   all  0.00604(2.533)  0.89946(1.506)
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.58680(1.697) -0.45336(2.489)  0.49811(1.681)  0.34757(3.660)
#>      year
#> age   5               6               7               8              
#>   all -1.09210(2.960) -0.64799(1.530)  0.44179(1.576) -0.10877(2.856)
#>      year
#> age   9               10             
#>   all -1.32152(1.913)  0.24407(2.573)
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
#>   all -0.024656(0.167)  0.151489(0.292) -0.150488(0.283) -0.242341(0.392)
#>      year
#> age   5                6                7                8               
#>   all -0.187069(0.425)  0.045611(0.281) -0.138436(0.756)  0.134168(0.450)
#>      year
#> age   9                10              
#>   all  0.311994(0.550)  0.144308(0.616)
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>      year
#> age   1                2                3                4               
#>   all  0.118168(0.437)  0.145382(0.384)  0.158438(0.298)  0.052861(0.233)
#>      year
#> age   5                6                7                8               
#>   all -0.169842(0.280) -0.034226(0.145) -0.430009(0.476)  0.323559(0.110)
#>      year
#> age   9                10              
#>   all -0.031262(0.431)  0.050915(0.384)
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.220965(0.642) -0.076462(0.259)  0.006771(0.357) -0.170477(0.611)
#>      year
#> age   5                6                7                8               
#>   all  0.201268(0.259) -0.104882(0.710)  0.007990(0.258) -0.316033(0.473)
#>      year
#> age   9                10              
#>   all  0.118366(0.585) -0.061203(0.677)
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.042138(0.529) -0.327569(0.319)  0.053334(0.286) -0.262403(0.396)
#>      year
#> age   5                6                7                8               
#>   all -0.141395(0.647) -0.303562(0.171)  0.282206(0.742)  0.026182(0.689)
#>      year
#> age   9                10              
#>   all  0.440035(0.404)  0.140852(0.297)
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>      year
#> age   1                2                3                4               
#>   all  0.045443(0.471) -0.155607(0.299) -0.000453(0.343)  0.071863(0.362)
#>      year
#> age   5                6                7                8               
#>   all -0.111596(0.358) -0.321763(0.280)  0.179524(0.258)  0.063306(0.544)
#>      year
#> age   9                10              
#>   all  0.361902(0.454)  0.010586(0.276)
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.047021(0.370) -0.002913(0.317) -0.228848(0.125) -0.131406(0.356)
#>      year
#> age   5                6                7                8               
#>   all  0.117077(0.649) -0.204569(0.372) -0.252387(0.377)  0.200146(0.420)
#>      year
#> age   9                10              
#>   all -0.068943(0.459) -0.513983(0.291)
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.097893(0.336)  0.180265(0.194)  0.161905(0.310)  0.155685(0.390)
#>      year
#> age   5                6                7                8               
#>   all  0.223090(0.227)  0.115159(0.386)  0.117655(0.448)  0.135816(0.294)
#>      year
#> age   9                10              
#>   all  0.001209(0.507)  0.179891(0.301)
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.117361(0.339) -0.090672(0.498)  0.099623(0.336)  0.069514(0.732)
#>      year
#> age   5                6                7                8               
#>   all -0.218420(0.592) -0.129598(0.306)  0.088358(0.315) -0.021754(0.571)
#>      year
#> age   9                10              
#>   all -0.264304(0.383)  0.048813(0.515)
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
#>   1 -1.3842(1.86)
#>   2  1.9586(1.38)
#>   3  2.5631(3.37)
#>   4  1.1530(4.26)
#>   5 -2.8516(3.91)
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>    year
#> age 1            
#>   1 -0.4246(3.03)
#>   2 -0.5895(3.90)
#>   3  0.2870(3.63)
#>   4  0.3166(5.99)
#>   5  0.2018(2.44)
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>    year
#> age 1            
#>   1 -2.7151(1.78)
#>   2 -0.2060(1.77)
#>   3 -0.5479(2.57)
#>   4 -2.0215(5.68)
#>   5  2.8370(2.93)
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>    year
#> age 1            
#>   1 -0.0585(1.87)
#>   2 -1.5524(3.67)
#>   3 -0.4943(3.86)
#>   4  1.1458(2.28)
#>   5  1.2911(4.94)
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>    year
#> age 1            
#>   1  1.7395(1.86)
#>   2 -1.1704(5.10)
#>   3  1.8992(4.08)
#>   4 -0.0142(2.16)
#>   5 -0.7062(4.01)
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>    year
#> age 1            
#>   1  0.9330(3.07)
#>   2 -0.7369(2.78)
#>   3 -1.9411(1.63)
#>   4  0.0953(3.97)
#>   5 -1.4099(2.50)
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>    year
#> age 1            
#>   1 -0.3415(3.08)
#>   2  0.7603(1.20)
#>   3  0.1737(3.10)
#>   4  0.0856(1.85)
#>   5  2.1489(1.31)
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>    year
#> age 1            
#>   1 -0.2072(2.51)
#>   2 -0.8949(3.10)
#>   3 -0.3104(2.37)
#>   4 -0.1906(2.28)
#>   5  0.7849(1.95)
#> 
#> units:  NA 
iterMeans(flq)
#> An object of class "FLQuant"
#> , , unit = 1, season = 1, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.070417 -0.300344 -0.299749 -0.472679 -0.235275 -0.219134 -0.378548
#>   2  0.250115  0.422029  0.456468  0.037765  0.152595  0.739375 -0.487723
#>   3  0.069156  0.014899 -0.027521 -0.265892  0.131514  0.323189  0.653340
#>   4  0.044422 -0.064799  0.056398 -0.249564 -0.187718 -0.141696  0.483715
#>   5 -0.280189  0.322687 -0.211957 -0.276660 -0.656901 -0.045724 -0.565094
#>    year
#> age 8         9         10       
#>   1  0.051886  0.288929 -0.129118
#>   2  0.010022  0.476566  0.305012
#>   3 -0.066499  0.586328  0.015075
#>   4  0.045724  0.017064  0.336588
#>   5  0.030968 -0.476350  0.096594
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.313409 -0.199397  0.344760 -0.248606 -0.396944  0.069165 -0.215428
#>   2  0.134784 -0.094059  0.334626  0.420501 -0.324866 -0.296372  0.030724
#>   3  0.172240  0.437694  0.245448  0.066976 -0.099387  0.126952 -0.303695
#>   4  0.236022 -0.098867  0.273866 -0.154328  0.070841 -0.046107 -0.357717
#>   5 -0.280872  0.316427 -0.166848 -0.153679 -0.003033  0.272707 -0.195319
#>    year
#> age 8         9         10       
#>   1  0.756790  0.177339 -0.451374
#>   2  0.379612 -0.040305 -0.156478
#>   3 -0.234742 -0.605752  0.462301
#>   4 -0.003343 -0.061881  0.775432
#>   5  0.251765 -0.228401 -0.233151
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.659334 -0.161187  0.064098  0.259621 -0.715189 -0.116724 -0.320908
#>   2 -0.124669  0.520940 -0.196613 -0.311611  0.541041 -0.165898 -0.172172
#>   3  0.432782 -0.758577  0.003532  0.157556  0.147694 -0.302154 -0.063312
#>   4 -0.510921 -0.016490 -0.340929 -0.362015  0.240755 -0.464093  0.338425
#>   5 -0.086262  0.805602 -0.167488  0.091434  0.617317 -0.026481  0.241995
#>    year
#> age 8         9         10       
#>   1 -0.009771  0.344595 -0.168526
#>   2 -0.565408  0.549580  0.058619
#>   3 -0.208369  0.389197  0.153848
#>   4 -0.464016 -0.262674 -0.404147
#>   5  0.337173 -0.050641  0.063375
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1  0.017317 -0.576714 -0.373536 -0.251078  0.333148 -0.066035  0.279832
#>   2 -0.326854 -0.298563 -0.244875  0.139003 -0.302591 -1.063523  0.528923
#>   3 -0.253999 -0.025375  0.606918 -0.329846 -0.779610 -0.070738 -0.307281
#>   4 -0.136433 -0.222283  0.080443  0.227293  0.003586 -0.211277  0.027908
#>   5 -0.049001 -0.243937  0.089547 -0.255441  0.097309  0.389937  0.341735
#>    year
#> age 8         9         10       
#>   1 -0.116134  0.352984  0.425534
#>   2 -0.178579  0.269698 -0.028570
#>   3 -0.026035  0.696365 -0.104625
#>   4 -0.294504  0.290428  0.499587
#>   5  0.489589  0.435826 -0.206435
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.395619 -0.104616 -0.074565  0.149591  0.376289  0.047132 -0.145179
#>   2 -0.457280 -0.484522  0.264028 -0.122115 -0.012777 -0.540651 -0.123843
#>   3 -0.183709  0.467280  0.539826  0.187869 -0.454229 -0.177971 -0.032665
#>   4  0.562078 -0.184988  0.099113  0.108094  0.118877 -0.604099  0.127912
#>   5  0.424327 -0.389716 -0.444377  0.323630  0.206065 -0.130102 -0.026219
#>    year
#> age 8         9         10       
#>   1  0.567746  0.178608  0.121444
#>   2 -0.368087  0.405036 -0.262025
#>   3  0.351441  0.439053 -0.069489
#>   4  0.272220  0.024885  0.123160
#>   5 -0.367377  0.130819  0.173265
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1  0.107940 -0.058604 -0.091239  0.000645  0.230337 -0.093480  0.213712
#>   2 -0.222435  0.338982 -0.692643  0.073077 -0.241033 -0.137528 -0.175946
#>   3 -0.005832 -0.106475 -0.093437  0.180245  0.050984 -0.376389 -0.819728
#>   4 -0.247385 -0.077452 -0.282321 -0.117467  0.271382  0.182121  0.027355
#>   5  0.083432 -0.259276 -0.088857 -0.075623 -0.105678 -0.259877 -0.206988
#>    year
#> age 8         9         10       
#>   1  0.374325 -0.109575 -0.412468
#>   2  0.399869  0.409190 -0.714665
#>   3 -0.407411 -0.239394 -0.405850
#>   4 -0.018434  0.359685 -0.443776
#>   5  0.282935 -0.314457 -0.375734
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.242035 -0.026071  0.103945  0.331129  0.241738 -0.303993  0.076859
#>   2  0.013479  0.132491  0.299681  0.335775 -0.134465 -0.298209 -0.106552
#>   3 -0.132768 -0.424959 -0.023317  0.163902  0.009407  0.792677 -0.123211
#>   4 -0.300697  0.089540  0.229452  0.050022  0.145177 -0.082810  0.073842
#>   5  0.156932  0.686532 -0.056481  0.199634  0.422044 -0.298540  0.145268
#>    year
#> age 8         9         10       
#>   1 -0.302723  0.400325 -0.684283
#>   2  0.392692 -0.126572  0.260068
#>   3  0.537351  0.425957 -0.263679
#>   4 -0.137022 -0.342997  0.443389
#>   5  0.262348  0.051308  0.705858
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.603186 -0.414420  0.269504 -0.056283  0.034171 -0.858951  0.020856
#>   2  0.063762 -0.337621 -0.007379  0.598281 -0.257099  0.107654 -0.111369
#>   3  0.161176  0.032981 -0.245664 -0.204468  0.344686 -0.536070 -0.041523
#>   4  0.231993 -0.121332  0.012404 -0.348672 -0.473929  0.542434  0.197094
#>   5 -0.013590  0.287098  0.318446 -0.217897 -0.538417  0.419164  0.166710
#>    year
#> age 8         9         10       
#>   1 -0.005524 -0.267172  0.412116
#>   2 -0.485825 -0.453444  0.115104
#>   3 -0.016296 -0.218243  0.030521
#>   4 -0.043863 -0.012219 -0.197048
#>   5  0.317009 -0.137699 -0.289231
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
#>   all    NA -1.33  1.40
#> 
#> units:  NA 
# although in fact a sum of no elements (as na.rm=TRUE) is zero
apply(x, 2:6, sum, na.rm=TRUE)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1     2     3    
#>   all  0.00 -1.33  1.40
#> 
#> units:  NA 
```
