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
#>   all -0.46369(2.274)  0.56303(1.013)  0.18924(2.458) -1.74430(2.021)
#>      year
#> age   5               6               7               8              
#>   all -0.11022(1.410) -0.00709(0.503) -1.41993(1.314) -0.02463(1.441)
#>      year
#> age   9               10             
#>   all -0.34062(1.819) -0.64213(1.568)
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.56899(1.837) -0.01460(1.696) -1.62454(1.602) -0.85133(1.592)
#>      year
#> age   5               6               7               8              
#>   all  0.81568(2.087) -0.45093(2.542)  0.20291(1.081)  1.14117(1.592)
#>      year
#> age   9               10             
#>   all -0.28786(3.175) -0.15259(1.271)
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all  0.21893(3.953)  0.34832(1.456)  0.21685(1.541) -0.22454(1.382)
#>      year
#> age   5               6               7               8              
#>   all -0.58297(1.491) -1.55339(1.882)  0.16100(0.911) -1.60406(2.579)
#>      year
#> age   9               10             
#>   all  0.09918(1.235) -0.55327(2.984)
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all  0.07605(0.918) -0.45349(1.558) -2.22126(1.027)  0.95165(2.412)
#>      year
#> age   5               6               7               8              
#>   all -1.15619(2.425) -0.44337(1.301) -0.37805(1.745)  0.05955(2.197)
#>      year
#> age   9               10             
#>   all  0.50459(3.092) -0.91269(3.953)
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.89795(2.226)  0.46881(2.048) -0.18784(2.072) -0.92032(4.341)
#>      year
#> age   5               6               7               8              
#>   all  0.44012(0.968)  0.46036(2.780) -0.32552(1.507)  1.72591(2.109)
#>      year
#> age   9               10             
#>   all -0.16640(2.009)  0.26856(2.987)
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -1.58463(2.328)  0.06879(1.689)  0.86750(2.221) -0.70469(1.077)
#>      year
#> age   5               6               7               8              
#>   all -1.94073(2.267)  1.04292(2.519) -0.41182(0.838)  1.07003(2.895)
#>      year
#> age   9               10             
#>   all -0.03841(2.184)  0.19105(1.398)
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.16310(1.537) -0.79474(1.605)  0.56427(1.642)  0.10274(2.464)
#>      year
#> age   5               6               7               8              
#>   all -0.15182(2.568) -0.73167(2.308)  0.81634(1.651)  0.23251(1.303)
#>      year
#> age   9               10             
#>   all -3.13608(1.222)  0.24197(1.957)
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all  1.38772(1.767)  0.68808(1.212)  0.94564(3.200)  0.19620(1.053)
#>      year
#> age   5               6               7               8              
#>   all -0.07879(2.646) -0.42818(2.884)  1.39508(1.267) -2.25749(1.818)
#>      year
#> age   9               10             
#>   all  0.50220(1.764) -0.57332(2.974)
#> 
#> units:  NA 
quantMeans(flq)
#> An object of class "FLQuant"
#> iters:  10 
#> 
#> , , unit = 1, season = 1, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.09274(0.455)  0.11261(0.203)  0.03785(0.492) -0.34886(0.404)
#>      year
#> age   5               6               7               8              
#>   all -0.02204(0.282) -0.00142(0.101) -0.28399(0.263) -0.00493(0.288)
#>      year
#> age   9               10             
#>   all -0.06812(0.364) -0.12843(0.314)
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.11380(0.367) -0.00292(0.339) -0.32491(0.320) -0.17027(0.318)
#>      year
#> age   5               6               7               8              
#>   all  0.16314(0.417) -0.09019(0.508)  0.04058(0.216)  0.22823(0.318)
#>      year
#> age   9               10             
#>   all -0.05757(0.635) -0.03052(0.254)
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all  0.04379(0.791)  0.06966(0.291)  0.04337(0.308) -0.04491(0.276)
#>      year
#> age   5               6               7               8              
#>   all -0.11659(0.298) -0.31068(0.376)  0.03220(0.182) -0.32081(0.516)
#>      year
#> age   9               10             
#>   all  0.01984(0.247) -0.11065(0.597)
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all  0.01521(0.184) -0.09070(0.312) -0.44425(0.205)  0.19033(0.482)
#>      year
#> age   5               6               7               8              
#>   all -0.23124(0.485) -0.08867(0.260) -0.07561(0.349)  0.01191(0.439)
#>      year
#> age   9               10             
#>   all  0.10092(0.618) -0.18254(0.791)
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.17959(0.445)  0.09376(0.410) -0.03757(0.414) -0.18406(0.868)
#>      year
#> age   5               6               7               8              
#>   all  0.08802(0.194)  0.09207(0.556) -0.06510(0.301)  0.34518(0.422)
#>      year
#> age   9               10             
#>   all -0.03328(0.402)  0.05371(0.597)
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.31693(0.466)  0.01376(0.338)  0.17350(0.444) -0.14094(0.215)
#>      year
#> age   5               6               7               8              
#>   all -0.38815(0.453)  0.20858(0.504) -0.08236(0.168)  0.21401(0.579)
#>      year
#> age   9               10             
#>   all -0.00768(0.437)  0.03821(0.280)
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.03262(0.307) -0.15895(0.321)  0.11285(0.328)  0.02055(0.493)
#>      year
#> age   5               6               7               8              
#>   all -0.03036(0.514) -0.14633(0.462)  0.16327(0.330)  0.04650(0.261)
#>      year
#> age   9               10             
#>   all -0.62722(0.244)  0.04839(0.391)
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all  0.27754(0.353)  0.13762(0.242)  0.18913(0.640)  0.03924(0.211)
#>      year
#> age   5               6               7               8              
#>   all -0.01576(0.529) -0.08564(0.577)  0.27902(0.253) -0.45150(0.364)
#>      year
#> age   9               10             
#>   all  0.10044(0.353) -0.11466(0.595)
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
#>   1 -0.0379(3.54)
#>   2 -0.8695(2.48)
#>   3  0.1838(3.47)
#>   4  0.3487(3.17)
#>   5 -0.6560(2.83)
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>    year
#> age 1            
#>   1  1.7313(1.91)
#>   2  2.3403(3.40)
#>   3  1.6782(2.17)
#>   4 -1.8059(3.13)
#>   5 -1.5497(3.21)
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>    year
#> age 1            
#>   1 -1.0549(2.60)
#>   2  0.9023(3.93)
#>   3 -0.3314(4.59)
#>   4 -0.0913(2.54)
#>   5 -1.6276(1.37)
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>    year
#> age 1            
#>   1 -0.2376(2.11)
#>   2 -1.3784(2.49)
#>   3 -1.0181(3.13)
#>   4  3.0580(4.11)
#>   5 -1.9166(4.68)
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>    year
#> age 1            
#>   1 -1.4070(2.87)
#>   2 -0.4421(3.83)
#>   3  0.8849(3.87)
#>   4  3.1497(2.46)
#>   5 -0.0948(2.97)
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>    year
#> age 1            
#>   1 -1.0808(3.72)
#>   2  1.9261(4.98)
#>   3 -1.0246(1.86)
#>   4 -0.7127(2.73)
#>   5  1.3310(2.04)
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>    year
#> age 1            
#>   1  0.1554(2.87)
#>   2 -2.5177(3.47)
#>   3 -0.3796(3.83)
#>   4 -1.5496(2.40)
#>   5  0.4413(3.00)
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>    year
#> age 1            
#>   1  1.0299(3.33)
#>   2  0.1364(2.63)
#>   3  1.0226(1.30)
#>   4  1.7791(1.87)
#>   5 -0.9349(1.56)
#> 
#> units:  NA 
iterMeans(flq)
#> An object of class "FLQuant"
#> , , unit = 1, season = 1, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.324515  0.152514  0.486270 -0.413509  0.020352  0.090242 -0.195276
#>   2 -0.052471 -0.336295 -0.267787  0.432163 -0.541296  0.081552 -0.081804
#>   3 -0.220790  0.017820 -0.288536 -0.355499  0.454496  0.429469 -0.040506
#>   4  0.471491  0.538973 -0.242209 -0.452660  0.422186  0.143762  0.209190
#>   5  0.152738  0.081877 -0.104961 -0.822607  0.046546  0.174228 -0.173929
#>    year
#> age 8         9         10       
#>   1 -0.356709  0.165414  0.250115
#>   2 -0.415330 -0.221063  0.069156
#>   3  0.049873  0.064117  0.044422
#>   4 -0.123985 -0.416068 -0.280189
#>   5  0.374679 -0.070417 -0.300344
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1  0.422029  0.456468  0.037765  0.152595  0.739375 -0.487723  0.010022
#>   2  0.014899 -0.027521 -0.265892  0.131514  0.323189  0.653340 -0.066499
#>   3 -0.064799  0.056398 -0.249564 -0.187718 -0.141696  0.483715  0.045724
#>   4  0.322687 -0.211957 -0.276660 -0.656901 -0.045724 -0.565094  0.030968
#>   5 -0.299749 -0.472679 -0.235275 -0.219134 -0.378548  0.051886  0.288929
#>    year
#> age 8         9         10       
#>   1  0.476566  0.305012  0.134784
#>   2  0.586328  0.015075  0.172240
#>   3  0.017064  0.336588  0.236022
#>   4 -0.476350  0.096594 -0.280872
#>   5 -0.129118 -0.313409 -0.199397
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.094059  0.334626  0.420501 -0.324866 -0.296372  0.030724  0.379612
#>   2  0.437694  0.245448  0.066976 -0.099387  0.126952 -0.303695 -0.234742
#>   3 -0.098867  0.273866 -0.154328  0.070841 -0.046107 -0.357717 -0.003343
#>   4  0.316427 -0.166848 -0.153679 -0.003033  0.272707 -0.195319  0.251765
#>   5  0.344760 -0.248606 -0.396944  0.069165 -0.215428  0.756790  0.177339
#>    year
#> age 8         9         10       
#>   1 -0.040305 -0.156478 -0.124669
#>   2 -0.605752  0.462301  0.432782
#>   3 -0.061881  0.775432 -0.510921
#>   4 -0.228401 -0.233151 -0.086262
#>   5 -0.451374 -0.659334 -0.161187
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1  0.520940 -0.196613 -0.311611  0.541041 -0.165898 -0.172172 -0.565408
#>   2 -0.758577  0.003532  0.157556  0.147694 -0.302154 -0.063312 -0.208369
#>   3 -0.016490 -0.340929 -0.362015  0.240755 -0.464093  0.338425 -0.464016
#>   4  0.805602 -0.167488  0.091434  0.617317 -0.026481  0.241995  0.337173
#>   5  0.064098  0.259621 -0.715189 -0.116724 -0.320908 -0.009771  0.344595
#>    year
#> age 8         9         10       
#>   1  0.549580  0.058619 -0.326854
#>   2  0.389197  0.153848 -0.253999
#>   3 -0.262674 -0.404147 -0.136433
#>   4 -0.050641  0.063375 -0.049001
#>   5 -0.168526  0.017317 -0.576714
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.298563 -0.244875  0.139003 -0.302591 -1.063523  0.528923 -0.178579
#>   2 -0.025375  0.606918 -0.329846 -0.779610 -0.070738 -0.307281 -0.026035
#>   3 -0.222283  0.080443  0.227293  0.003586 -0.211277  0.027908 -0.294504
#>   4 -0.243937  0.089547 -0.255441  0.097309  0.389937  0.341735  0.489589
#>   5 -0.373536 -0.251078  0.333148 -0.066035  0.279832 -0.116134  0.352984
#>    year
#> age 8         9         10       
#>   1  0.269698 -0.028570 -0.457280
#>   2  0.696365 -0.104625 -0.183709
#>   3  0.290428  0.499587  0.562078
#>   4  0.435826 -0.206435  0.424327
#>   5  0.425534 -0.395619 -0.104616
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.484522  0.264028 -0.122115 -0.012777 -0.540651 -0.123843 -0.368087
#>   2  0.467280  0.539826  0.187869 -0.454229 -0.177971 -0.032665  0.351441
#>   3 -0.184988  0.099113  0.108094  0.118877 -0.604099  0.127912  0.272220
#>   4 -0.389716 -0.444377  0.323630  0.206065 -0.130102 -0.026219 -0.367377
#>   5 -0.074565  0.149591  0.376289  0.047132 -0.145179  0.567746  0.178608
#>    year
#> age 8         9         10       
#>   1  0.405036 -0.262025 -0.222435
#>   2  0.439053 -0.069489 -0.005832
#>   3  0.024885  0.123160 -0.247385
#>   4  0.130819  0.173265  0.083432
#>   5  0.121444  0.107940 -0.058604
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1  0.338982 -0.692643  0.073077 -0.241033 -0.137528 -0.175946  0.399869
#>   2 -0.106475 -0.093437  0.180245  0.050984 -0.376389 -0.819728 -0.407411
#>   3 -0.077452 -0.282321 -0.117467  0.271382  0.182121  0.027355 -0.018434
#>   4 -0.259276 -0.088857 -0.075623 -0.105678 -0.259877 -0.206988  0.282935
#>   5 -0.091239  0.000645  0.230337 -0.093480  0.213712  0.374325 -0.109575
#>    year
#> age 8         9         10       
#>   1  0.409190 -0.714665  0.013479
#>   2 -0.239394 -0.405850 -0.132768
#>   3  0.359685 -0.443776 -0.300697
#>   4 -0.314457 -0.375734  0.156932
#>   5 -0.412468 -0.242035 -0.026071
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1  0.132491  0.299681  0.335775 -0.134465 -0.298209 -0.106552  0.392692
#>   2 -0.424959 -0.023317  0.163902  0.009407  0.792677 -0.123211  0.537351
#>   3  0.089540  0.229452  0.050022  0.145177 -0.082810  0.073842 -0.137022
#>   4  0.686532 -0.056481  0.199634  0.422044 -0.298540  0.145268  0.262348
#>   5  0.103945  0.331129  0.241738 -0.303993  0.076859 -0.302723  0.400325
#>    year
#> age 8         9         10       
#>   1 -0.126572  0.260068  0.063762
#>   2  0.425957 -0.263679  0.161176
#>   3 -0.342997  0.443389  0.231993
#>   4  0.051308  0.705858 -0.013590
#>   5 -0.684283 -0.603186 -0.414420
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
#> quant 1      2      3     
#>   all     NA  1.717 -0.556
#> 
#> units:  NA 
# although in fact a sum of no elements (as na.rm=TRUE) is zero
apply(x, 2:6, sum, na.rm=TRUE)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1      2      3     
#>   all  0.000  1.717 -0.556
#> 
#> units:  NA 
```
