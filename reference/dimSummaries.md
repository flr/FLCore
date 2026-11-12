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
#>   all -1.07299(3.410) -0.26613(1.422)  0.28727(1.216) -0.70436(2.426)
#>      year
#> age   5               6               7               8              
#>   all -0.10063(2.094) -2.28558(1.916)  0.13153(1.647)  0.57119(1.728)
#>      year
#> age   9               10             
#>   all -0.30860(1.866) -0.66068(1.076)
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all -1.06493(2.833)  1.51784(1.549) -1.25423(1.850) -0.53792(1.293)
#>      year
#> age   5               6               7               8              
#>   all  0.42752(1.495) -1.29087(3.234) -0.52426(1.692) -1.04441(2.424)
#>      year
#> age   9               10             
#>   all -0.60464(1.085)  0.44870(2.148)
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.82787(2.749) -1.92179(3.438)  0.61195(3.032) -0.10877(2.792)
#>      year
#> age   5               6               7               8              
#>   all  1.08057(2.741)  1.44222(2.536) -0.02777(1.344)  0.35331(2.153)
#>      year
#> age   9               10             
#>   all  0.12826(2.171)  0.34410(1.807)
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>      year
#> age   1               2               3               4              
#>   all  0.71726(2.144) -1.27670(2.134) -1.52446(1.526)  1.17719(1.946)
#>      year
#> age   5               6               7               8              
#>   all  0.78161(0.893)  0.34679(1.031)  0.04506(3.274)  0.35191(0.820)
#>      year
#> age   9               10             
#>   all -1.36315(1.906) -0.58246(2.506)
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.13393(2.410)  0.26990(2.267)  0.12719(0.571) -0.02030(0.799)
#>      year
#> age   5               6               7               8              
#>   all  0.13308(1.478) -1.46739(1.642) -1.95947(3.525)  0.45146(0.474)
#>      year
#> age   9               10             
#>   all  0.77984(1.661)  0.62601(1.044)
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -0.55604(2.401) -0.77450(2.922)  0.91683(1.936)  1.34651(1.384)
#>      year
#> age   5               6               7               8              
#>   all  0.57882(1.697) -1.14640(1.308)  0.20458(2.032)  0.47206(2.345)
#>      year
#> age   9               10             
#>   all  0.58190(2.226)  1.13108(2.053)
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all  0.13178(1.692) -1.26571(1.805) -0.13551(1.712)  0.37236(1.747)
#>      year
#> age   5               6               7               8              
#>   all -0.17158(2.990)  0.08341(1.554) -0.24550(2.007)  0.77368(2.000)
#>      year
#> age   9               10             
#>   all -0.03667(2.139)  0.03450(1.117)
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>      year
#> age   1               2               3               4              
#>   all -1.25265(2.148)  0.14564(1.251) -1.52551(2.571)  0.00568(3.669)
#>      year
#> age   5               6               7               8              
#>   all  1.37262(2.254)  0.95533(1.066)  0.77761(3.107) -1.26287(2.822)
#>      year
#> age   9               10             
#>   all  0.75002(3.591) -0.40634(2.912)
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
#>   all -0.21460(0.6820) -0.05323(0.2845)  0.05745(0.2432) -0.14087(0.4852)
#>      year
#> age   5                6                7                8               
#>   all -0.02013(0.4189) -0.45712(0.3832)  0.02631(0.3293)  0.11424(0.3456)
#>      year
#> age   9                10              
#>   all -0.06172(0.3731) -0.13214(0.2152)
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.21299(0.5666)  0.30357(0.3097) -0.25085(0.3699) -0.10758(0.2586)
#>      year
#> age   5                6                7                8               
#>   all  0.08550(0.2990) -0.25817(0.6467) -0.10485(0.3385) -0.20888(0.4847)
#>      year
#> age   9                10              
#>   all -0.12093(0.2169)  0.08974(0.4296)
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.16557(0.5498) -0.38436(0.6876)  0.12239(0.6065) -0.02175(0.5583)
#>      year
#> age   5                6                7                8               
#>   all  0.21611(0.5483)  0.28844(0.5072) -0.00555(0.2687)  0.07066(0.4307)
#>      year
#> age   9                10              
#>   all  0.02565(0.4342)  0.06882(0.3614)
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>      year
#> age   1                2                3                4               
#>   all  0.14345(0.4288) -0.25534(0.4269) -0.30489(0.3052)  0.23544(0.3893)
#>      year
#> age   5                6                7                8               
#>   all  0.15632(0.1787)  0.06936(0.2061)  0.00901(0.6547)  0.07038(0.1641)
#>      year
#> age   9                10              
#>   all -0.27263(0.3811) -0.11649(0.5013)
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.02679(0.4820)  0.05398(0.4534)  0.02544(0.1142) -0.00406(0.1598)
#>      year
#> age   5                6                7                8               
#>   all  0.02662(0.2957) -0.29348(0.3283) -0.39189(0.7051)  0.09029(0.0948)
#>      year
#> age   9                10              
#>   all  0.15597(0.3322)  0.12520(0.2089)
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.11121(0.4802) -0.15490(0.5843)  0.18337(0.3873)  0.26930(0.2769)
#>      year
#> age   5                6                7                8               
#>   all  0.11576(0.3393) -0.22928(0.2615)  0.04092(0.4064)  0.09441(0.4690)
#>      year
#> age   9                10              
#>   all  0.11638(0.4451)  0.22622(0.4105)
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>      year
#> age   1                2                3                4               
#>   all  0.02636(0.3385) -0.25314(0.3609) -0.02710(0.3423)  0.07447(0.3494)
#>      year
#> age   5                6                7                8               
#>   all -0.03432(0.5980)  0.01668(0.3108) -0.04910(0.4014)  0.15474(0.4000)
#>      year
#> age   9                10              
#>   all -0.00733(0.4278)  0.00690(0.2234)
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>      year
#> age   1                2                3                4               
#>   all -0.25053(0.4296)  0.02913(0.2502) -0.30510(0.5142)  0.00114(0.7338)
#>      year
#> age   5                6                7                8               
#>   all  0.27452(0.4508)  0.19107(0.2131)  0.15552(0.6213) -0.25257(0.5645)
#>      year
#> age   9                10              
#>   all  0.15000(0.7182) -0.08127(0.5824)
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
#>   1  0.6116(2.28)
#>   2  0.9013(4.61)
#>   3  0.3360(2.26)
#>   4 -0.5222(3.36)
#>   5 -0.7182(2.45)
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>    year
#> age 1            
#>   1  0.2005(3.68)
#>   2 -1.0955(2.46)
#>   3  1.8413(4.15)
#>   4 -2.0568(4.88)
#>   5 -0.3447(2.43)
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>    year
#> age 1            
#>   1  0.2526(3.13)
#>   2  0.8998(7.35)
#>   3  1.3658(3.32)
#>   4  1.0080(3.23)
#>   5 -1.4665(1.63)
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>    year
#> age 1            
#>   1  0.2531(3.71)
#>   2 -1.0857(1.25)
#>   3 -0.9740(1.74)
#>   4  2.0627(2.35)
#>   5 -0.5178(3.19)
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>    year
#> age 1            
#>   1 -2.7973(3.79)
#>   2  0.9086(4.94)
#>   3 -0.0455(2.91)
#>   4  0.9807(3.57)
#>   5  0.2802(2.56)
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>    year
#> age 1            
#>   1  0.2219(3.24)
#>   2  0.4267(2.46)
#>   3  2.2474(4.49)
#>   4 -1.6859(1.48)
#>   5  1.4291(2.18)
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>    year
#> age 1            
#>   1 -1.1544(1.34)
#>   2 -0.3391(2.04)
#>   3 -1.1395(2.25)
#>   4 -0.0394(2.41)
#>   5 -0.2690(3.56)
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>    year
#> age 1            
#>   1  2.6571(1.68)
#>   2 -0.4715(3.46)
#>   3 -1.5680(4.84)
#>   4 -0.7918(3.53)
#>   5  0.5916(3.99)
#> 
#> units:  NA 
iterMeans(flq)
#> An object of class "FLQuant"
#> , , unit = 1, season = 1, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1  0.066976 -0.099387  0.126952 -0.303695 -0.234742 -0.605752  0.462301
#>   2 -0.154328  0.070841 -0.046107 -0.357717 -0.003343 -0.061881  0.775432
#>   3 -0.153679 -0.003033  0.272707 -0.195319  0.251765 -0.228401 -0.233151
#>   4 -0.396944  0.069165 -0.215428  0.756790  0.177339 -0.451374 -0.659334
#>   5 -0.324866 -0.296372  0.030724  0.379612 -0.040305 -0.156478 -0.124669
#>    year
#> age 8         9         10       
#>   1  0.432782 -0.758577  0.003532
#>   2 -0.510921 -0.016490 -0.340929
#>   3 -0.086262  0.805602 -0.167488
#>   4 -0.161187  0.064098  0.259621
#>   5  0.520940 -0.196613 -0.311611
#> 
#> , , unit = 2, season = 1, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1  0.157556  0.147694 -0.302154 -0.063312 -0.208369  0.389197  0.153848
#>   2 -0.362015  0.240755 -0.464093  0.338425 -0.464016 -0.262674 -0.404147
#>   3  0.091434  0.617317 -0.026481  0.241995  0.337173 -0.050641  0.063375
#>   4 -0.715189 -0.116724 -0.320908 -0.009771  0.344595 -0.168526  0.017317
#>   5  0.541041 -0.165898 -0.172172 -0.565408  0.549580  0.058619 -0.326854
#>    year
#> age 8         9         10       
#>   1 -0.253999 -0.025375  0.606918
#>   2 -0.136433 -0.222283  0.080443
#>   3 -0.049001 -0.243937  0.089547
#>   4 -0.576714 -0.373536 -0.251078
#>   5 -0.298563 -0.244875  0.139003
#> 
#> , , unit = 1, season = 2, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.329846 -0.779610 -0.070738 -0.307281 -0.026035  0.696365 -0.104625
#>   2  0.227293  0.003586 -0.211277  0.027908 -0.294504  0.290428  0.499587
#>   3 -0.255441  0.097309  0.389937  0.341735  0.489589  0.435826 -0.206435
#>   4  0.333148 -0.066035  0.279832 -0.116134  0.352984  0.425534 -0.395619
#>   5 -0.302591 -1.063523  0.528923 -0.178579  0.269698 -0.028570 -0.457280
#>    year
#> age 8         9         10       
#>   1 -0.183709  0.467280  0.539826
#>   2  0.562078 -0.184988  0.099113
#>   3  0.424327 -0.389716 -0.444377
#>   4 -0.104616 -0.074565  0.149591
#>   5 -0.484522  0.264028 -0.122115
#> 
#> , , unit = 2, season = 2, area = 1
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1  0.187869 -0.454229 -0.177971 -0.032665  0.351441  0.439053 -0.069489
#>   2  0.108094  0.118877 -0.604099  0.127912  0.272220  0.024885  0.123160
#>   3  0.323630  0.206065 -0.130102 -0.026219 -0.367377  0.130819  0.173265
#>   4  0.376289  0.047132 -0.145179  0.567746  0.178608  0.121444  0.107940
#>   5 -0.012777 -0.540651 -0.123843 -0.368087  0.405036 -0.262025 -0.222435
#>    year
#> age 8         9         10       
#>   1 -0.005832 -0.106475 -0.093437
#>   2 -0.247385 -0.077452 -0.282321
#>   3  0.083432 -0.259276 -0.088857
#>   4 -0.058604 -0.091239  0.000645
#>   5  0.338982 -0.692643  0.073077
#> 
#> , , unit = 1, season = 1, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1  0.180245  0.050984 -0.376389 -0.819728 -0.407411 -0.239394 -0.405850
#>   2 -0.117467  0.271382  0.182121  0.027355 -0.018434  0.359685 -0.443776
#>   3 -0.075623 -0.105678 -0.259877 -0.206988  0.282935 -0.314457 -0.375734
#>   4  0.230337 -0.093480  0.213712  0.374325 -0.109575 -0.412468 -0.242035
#>   5 -0.241033 -0.137528 -0.175946  0.399869  0.409190 -0.714665  0.013479
#>    year
#> age 8         9         10       
#>   1 -0.132768 -0.424959 -0.023317
#>   2 -0.300697  0.089540  0.229452
#>   3  0.156932  0.686532 -0.056481
#>   4 -0.026071  0.103945  0.331129
#>   5  0.132491  0.299681  0.335775
#> 
#> , , unit = 2, season = 1, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1  0.163902  0.009407  0.792677 -0.123211  0.537351  0.425957 -0.263679
#>   2  0.050022  0.145177 -0.082810  0.073842 -0.137022 -0.342997  0.443389
#>   3  0.199634  0.422044 -0.298540  0.145268  0.262348  0.051308  0.705858
#>   4  0.241738 -0.303993  0.076859 -0.302723  0.400325 -0.684283 -0.603186
#>   5 -0.134465 -0.298209 -0.106552  0.392692 -0.126572  0.260068  0.063762
#>    year
#> age 8         9         10       
#>   1  0.161176  0.032981 -0.245664
#>   2  0.231993 -0.121332  0.012404
#>   3 -0.013590  0.287098  0.318446
#>   4 -0.414420  0.269504 -0.056283
#>   5 -0.337621 -0.007379  0.598281
#> 
#> , , unit = 1, season = 2, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.204468  0.344686 -0.536070 -0.041523 -0.016296 -0.218243  0.030521
#>   2 -0.348672 -0.473929  0.542434  0.197094 -0.043863 -0.012219 -0.197048
#>   3 -0.217897 -0.538417  0.419164  0.166710  0.317009 -0.137699 -0.289231
#>   4  0.034171 -0.858951  0.020856 -0.005524 -0.267172  0.412116  0.147309
#>   5 -0.257099  0.107654 -0.111369 -0.485825 -0.453444  0.115104 -0.141699
#>    year
#> age 8         9         10       
#>   1  0.060520 -0.001328 -0.147361
#>   2  0.187045  0.064980  0.193907
#>   3 -0.166311  0.130171  0.060895
#>   4 -0.294917 -0.378188 -0.190439
#>   5  0.690690  0.129141  0.206383
#> 
#> , , unit = 2, season = 2, area = 2
#> 
#>    year
#> age 1         2         3         4         5         6         7        
#>   1 -0.053811  0.246123  0.166434  0.409121 -0.121034  0.649053  0.290859
#>   2 -0.318792 -0.422570 -0.028207  0.344808  0.052656  0.038505  0.044410
#>   3 -0.512405 -0.693082 -0.080637 -0.572283  0.191731 -0.276475  0.420755
#>   4 -0.225714 -0.144508 -0.547615  0.025542  0.307477 -0.244550 -0.102971
#>   5  0.222203  0.619937 -0.559777 -0.047781  0.463654  0.277837  0.086710
#>    year
#> age 8         9         10       
#>   1  0.079030  0.162075  0.132517
#>   2 -0.186043  0.047501  0.111570
#>   3 -0.214412  0.401054 -0.224196
#>   4 -0.282395  0.488016 -0.149089
#>   5 -0.187889  0.350749  0.312746
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
#>   all     NA -0.011  0.208
#> 
#> units:  NA 
# although in fact a sum of no elements (as na.rm=TRUE) is zero
apply(x, 2:6, sum, na.rm=TRUE)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1      2      3     
#>   all  0.000 -0.011  0.208
#> 
#> units:  NA 
```
