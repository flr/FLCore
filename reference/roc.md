# Receiver Operating Characteristic table with True Skill Statistic (TSS)

A receiver operating characteristic (ROC) curve shows the ability of a
binary classifier. Here it is applied to compare two sets of values,
stored as two FLQuant objects. The first is the result of aplying a
logical comparison of a given state against a reference value, so it
contains a binary (0, 1) label. The second, the score, contains an
alternative metric that attempts to measure the absolute value of the
first. The examples below compare an observation of stock status, SSB
being less than a reference point, and an alternative metric, here the
catch curve estimates of total mortality.

## Usage

``` r
roc(label, ind, direction = c(">=", "<="))

auc(x = NULL, TPR = x$TPR, FPR = x$FPR)
```

## Arguments

- label:

  Logical, integer (0/1), or FLQuant giving the true class for each
  observation (1 = positive, 0 = negative). Non-logical values are
  coerced to 0/1. Labels must not be all 0 or all 1.

- ind:

  Numeric vector or FLQuant of indicator / score values used to rank
  observations.

- direction:

  Character scalar, one of `">="` (default) or `"<="`. If `">="`, larger
  `ind` values are treated as more evidence for the positive class; if
  `"<="`, smaller `ind` values are treated as more evidence for the
  positive class.

## Value

A data.frame (model.frame output) sorted by the chosen threshold order
containing the columns:

- ind:

  indicator / score values

- label:

  coerced 0/1 label

- TP, TN, FP, FN:

  cumulative true/false positive/negative counts

- TPR, FPR:

  true positive rate and false positive rate

- TSS:

  True Skill Statistic, computed as TPR - FPR (i.e. tp/(tp+fn) -
  fp/(fp+tn))

## Details

When `label` and `ind` are FLQuant objects the function will propagate
them along the 6th dimension if needed. The function checks that `label`
contains only 0/1 and that both arguments have matching dimensions.
Observations are ordered according to `ind` (respecting `direction`) and
cumulative counts and rates are computed.

## Examples

``` r
data(ple4)
# OM 'reality' on stock status (fbar)
state <- fbar(ple4)[, ac(1960:2017)]
# Model estimates of F using catch curves
ind <- acc(catch.n(ple4)[, ac(1960:2017)])
# Compute TSS, returns data.frame
roc(state >= 0.22, ind)
#>    year       ind label TP TN FP FN        TPR       FPR          TSS
#> 13 1972 0.2286824     1  1  9  0 48 0.02040816 0.0000000  0.020408163
#> 12 1971 0.2692419     1  2  9  0 47 0.04081633 0.0000000  0.040816327
#> 9  1968 0.3031928     1  3  9  0 46 0.06122449 0.0000000  0.061224490
#> 8  1967 0.3094249     1  4  9  0 45 0.08163265 0.0000000  0.081632653
#> 11 1970 0.3135998     1  5  9  0 44 0.10204082 0.0000000  0.102040816
#> 10 1969 0.3153705     1  6  9  0 43 0.12244898 0.0000000  0.122448980
#> 4  1963 0.3231395     1  7  9  0 42 0.14285714 0.0000000  0.142857143
#> 14 1973 0.3253669     1  8  9  0 41 0.16326531 0.0000000  0.163265306
#> 7  1966 0.3260040     1  9  9  0 40 0.18367347 0.0000000  0.183673469
#> 6  1965 0.3455516     1 10  9  0 39 0.20408163 0.0000000  0.204081633
#> 1  1960 0.3471932     1 11  9  0 38 0.22448980 0.0000000  0.224489796
#> 3  1962 0.3489301     1 12  9  0 37 0.24489796 0.0000000  0.244897959
#> 2  1961 0.3596979     1 13  9  0 36 0.26530612 0.0000000  0.265306122
#> 5  1964 0.3806223     1 14  9  0 35 0.28571429 0.0000000  0.285714286
#> 15 1974 0.3944662     1 15  9  0 34 0.30612245 0.0000000  0.306122449
#> 35 1994 0.4238022     1 16  9  0 33 0.32653061 0.0000000  0.326530612
#> 58 2017 0.4276845     0 16  8  1 33 0.32653061 0.1111111  0.215419501
#> 57 2016 0.4380586     0 16  7  2 33 0.32653061 0.2222222  0.104308390
#> 16 1975 0.4400404     1 17  7  2 32 0.34693878 0.2222222  0.124716553
#> 34 1993 0.4438759     1 18  7  2 31 0.36734694 0.2222222  0.145124717
#> 17 1976 0.4608337     1 19  7  2 30 0.38775510 0.2222222  0.165532880
#> 36 1995 0.4642887     1 20  7  2 29 0.40816327 0.2222222  0.185941043
#> 56 2015 0.4644625     0 20  6  3 29 0.40816327 0.3333333  0.074829932
#> 22 1981 0.4791809     1 21  6  3 28 0.42857143 0.3333333  0.095238095
#> 37 1996 0.4907357     1 22  6  3 27 0.44897959 0.3333333  0.115646259
#> 18 1977 0.4931742     1 23  6  3 26 0.46938776 0.3333333  0.136054422
#> 33 1992 0.5010075     1 24  6  3 25 0.48979592 0.3333333  0.156462585
#> 21 1980 0.5080961     1 25  6  3 24 0.51020408 0.3333333  0.176870748
#> 55 2014 0.5135529     0 25  5  4 24 0.51020408 0.4444444  0.065759637
#> 19 1978 0.5136786     1 26  5  4 23 0.53061224 0.4444444  0.086167800
#> 54 2013 0.5148835     0 26  4  5 23 0.53061224 0.5555556 -0.024943311
#> 20 1979 0.5205015     1 27  4  5 22 0.55102041 0.5555556 -0.004535147
#> 53 2012 0.5211683     0 27  3  6 22 0.55102041 0.6666667 -0.115646259
#> 23 1982 0.5334924     1 28  3  6 21 0.57142857 0.6666667 -0.095238095
#> 32 1991 0.5479495     1 29  3  6 20 0.59183673 0.6666667 -0.074829932
#> 38 1997 0.5721862     1 30  3  6 19 0.61224490 0.6666667 -0.054421769
#> 24 1983 0.5727272     1 31  3  6 18 0.63265306 0.6666667 -0.034013605
#> 52 2011 0.5756761     0 31  2  7 18 0.63265306 0.7777778 -0.145124717
#> 31 1990 0.5793923     1 32  2  7 17 0.65306122 0.7777778 -0.124716553
#> 39 1998 0.5870266     1 33  2  7 16 0.67346939 0.7777778 -0.104308390
#> 25 1984 0.5948203     1 34  2  7 15 0.69387755 0.7777778 -0.083900227
#> 51 2010 0.5961145     0 34  1  8 15 0.69387755 0.8888889 -0.195011338
#> 40 1999 0.6073061     1 35  1  8 14 0.71428571 0.8888889 -0.174603175
#> 30 1989 0.6090999     1 36  1  8 13 0.73469388 0.8888889 -0.154195011
#> 26 1985 0.6143431     1 37  1  8 12 0.75510204 0.8888889 -0.133786848
#> 29 1988 0.6314991     1 38  1  8 11 0.77551020 0.8888889 -0.113378685
#> 50 2009 0.6373167     0 38  0  9 11 0.77551020 1.0000000 -0.224489796
#> 41 2000 0.6438515     1 39  0  9 10 0.79591837 1.0000000 -0.204081633
#> 47 2006 0.6459748     1 40  0  9  9 0.81632653 1.0000000 -0.183673469
#> 46 2005 0.6476648     1 41  0  9  8 0.83673469 1.0000000 -0.163265306
#> 28 1987 0.6534843     1 42  0  9  7 0.85714286 1.0000000 -0.142857143
#> 42 2001 0.6574388     1 43  0  9  6 0.87755102 1.0000000 -0.122448980
#> 49 2008 0.6596211     1 44  0  9  5 0.89795918 1.0000000 -0.102040816
#> 48 2007 0.6657043     1 45  0  9  4 0.91836735 1.0000000 -0.081632653
#> 27 1986 0.6719858     1 46  0  9  3 0.93877551 1.0000000 -0.061224490
#> 44 2003 0.6727245     1 47  0  9  2 0.95918367 1.0000000 -0.040816327
#> 45 2004 0.6789889     1 48  0  9  1 0.97959184 1.0000000 -0.020408163
#> 43 2002 0.7097662     1 49  0  9  0 1.00000000 1.0000000  0.000000000
# Needs ggplot2
if (FALSE) { # \dontrun{
ggplot(roc(state >= 0.22, ind, direction='>='), aes(x=FPR, y=TPR)) +
  geom_line() +
  geom_abline(slope=1, intercept=0, colour="red", linetype=2)
} # }
# Computes auc using the output of roc()
with(roc(state >= 0.22, ind), auc(TPR=TPR, FPR=FPR))
#> [1] 0.5283447
auc(roc(state >= 0.22, ind))
#> [1] 0.5283447
```
