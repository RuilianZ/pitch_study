Mixed Effects Model for a Pitch Study
================
Roxy Zhang
4/6/2022

``` r
data = read.csv("data.csv")

summary(data)
```

    ##    subject             gender             scenario   attitude        
    ##  Length:84          Length:84          Min.   :1   Length:84         
    ##  Class :character   Class :character   1st Qu.:2   Class :character  
    ##  Mode  :character   Mode  :character   Median :4   Mode  :character  
    ##                                        Mean   :4                     
    ##                                        3rd Qu.:6                     
    ##                                        Max.   :7                     
    ##    frequency    
    ##  Min.   : 82.2  
    ##  1st Qu.:126.8  
    ##  Median :201.8  
    ##  Mean   :192.6  
    ##  3rd Qu.:247.4  
    ##  Max.   :306.8

``` r
boxplot(frequency ~ attitude*gender, data = data)
```

![](pitch_study_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
library(lme4)
```

    ## Loading required package: Matrix

``` r
# (b)
m1 = lmer(frequency ~ gender + attitude + (1|subject), REML = F, data = data)

summary(m1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: frequency ~ gender + attitude + (1 | subject)
    ##    Data: data
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    825.6    837.8   -407.8    815.6       79 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3945 -0.5728 -0.2229  0.4872  3.3291 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  subject  (Intercept) 379.4    19.48   
    ##  Residual             836.8    28.93   
    ## Number of obs: 84, groups:  subject, 6
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  256.987     12.504  20.552
    ## genderM     -108.798     17.111  -6.358
    ## attitudepol  -20.002      6.313  -3.169
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) gendrM
    ## genderM     -0.684       
    ## attitudepol -0.252  0.000

``` r
round(ranef(m1)$subject, 3)
```

    ##    (Intercept)
    ## F1     -12.915
    ## F2       9.676
    ## F3       3.240
    ## M3      26.600
    ## M4       4.509
    ## M7     -31.108

``` r
round(residuals(m1), 3)
```

    ##       1       2       3       4       5       6       7       8       9      10 
    ## -10.769 -39.572  61.031  15.628 -20.169  42.828  26.731  32.728   7.831   8.328 
    ##      11      12      13      14      15      16      17      18      19      20 
    ## -42.869 -13.372 -27.572 -69.269 -10.524 -22.926  -3.424  -9.226  26.776   5.774 
    ##      21      22      23      24      25      26      27      28      29      30 
    ##  35.176  46.574  -7.624  -7.726 -13.724  18.574   4.174 -54.724 -21.996 -29.098 
    ##      31      32      33      34      35      36      37      38      39      40 
    ##  96.304 -37.798 -20.496  60.902  60.704  10.202 -30.896 -25.798 -22.696 -16.498 
    ##      41      42      43      44      45      46      47      48      49      50 
    ##  -6.698  -6.196 -10.979 -17.981 -14.879 -12.781 -11.179  -6.881   0.021   2.919 
    ##      51      52      53      54      55      56      57      58      59      60 
    ##  -3.379 -14.181  11.721  -8.881   7.319  10.521 -13.960 -35.362  -0.360  -6.962 
    ##      61      62      63      64      65      66      67      68      69      70 
    ##  42.740  35.138  -3.460  29.538  31.040  27.538 -38.660 -40.762  14.338 -19.460 
    ##      71      72      73      74      75      76      77      78      79      80 
    ##  -0.987  14.011 -12.387  24.911   5.413  11.311  52.713  16.111   5.913 -18.289 
    ##      81      82      83      84 
    ##  -8.087 -16.789 -13.689  -1.487

``` r
# (c)
m2 = lmer(frequency ~ gender + attitude + gender*attitude+ (1|subject),
REML = F, data = data)

summary(m2)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: frequency ~ gender + attitude + gender * attitude + (1 | subject)
    ##    Data: data
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    826.3    840.8   -407.1    814.3       78 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2856 -0.5246 -0.1719  0.4929  3.2294 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  subject  (Intercept) 380.4    19.50   
    ##  Residual             822.1    28.67   
    ## Number of obs: 84, groups:  subject, 6
    ## 
    ## Fixed effects:
    ##                     Estimate Std. Error t value
    ## (Intercept)          260.686     12.883  20.235
    ## genderM             -116.195     18.219  -6.378
    ## attitudepol          -27.400      8.848  -3.097
    ## genderM:attitudepol   14.795     12.514   1.182
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) gendrM atttdp
    ## genderM     -0.707              
    ## attitudepol -0.343  0.243       
    ## gndrM:tttdp  0.243 -0.343 -0.707

``` r
round(ranef(m2)$subject, 3)
```

    ##    (Intercept)
    ## F1     -12.951
    ## F2       9.702
    ## F3       3.249
    ## M3      26.673
    ## M4       4.521
    ## M7     -31.195

``` r
stats = deviance(m1) - deviance(m2)
 
p_value = 1 - pchisq(stats, 1)

# (d)
m3 <- lmer(frequency ~ gender + attitude + (1+attitude|subject), data = data)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
summary(m3)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: frequency ~ gender + attitude + (1 + attitude | subject)
    ##    Data: data
    ## 
    ## REML criterion at convergence: 796.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3482 -0.5751 -0.2125  0.4770  3.2953 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr
    ##  subject  (Intercept) 578.7865 24.0580      
    ##           attitudepol   0.6511  0.8069  1.00
    ##  Residual             847.5471 29.1127      
    ## Number of obs: 84, groups:  subject, 6
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  257.588     15.040  17.127
    ## genderM     -110.000     20.933  -5.255
    ## attitudepol  -20.002      6.361  -3.144
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) gendrM
    ## genderM     -0.696       
    ## attitudepol -0.177  0.000
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
ranef(m3)
```

    ## $subject
    ##    (Intercept) attitudepol
    ## F1  -13.956088 -0.46807765
    ## F2    9.436867  0.31650606
    ## F3    2.744934  0.09206321
    ## M3   28.055212  0.94095263
    ## M4    5.301848  0.17782035
    ## M7  -31.582772 -1.05926460
    ## 
    ## with conditional variances for "subject"

``` r
m4 <- lmer(frequency ~ gender + attitude + (1|subject)+(1|scenario), data = data)
summary(m4)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: frequency ~ gender + attitude + (1 | subject) + (1 | scenario)
    ##    Data: data
    ## 
    ## REML criterion at convergence: 784.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.2690 -0.6331 -0.0878  0.5204  3.5326 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  scenario (Intercept) 224.5    14.98   
    ##  subject  (Intercept) 613.2    24.76   
    ##  Residual             637.8    25.25   
    ## Number of obs: 84, groups:  scenario, 7; subject, 6
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  256.987     16.101  15.961
    ## genderM     -108.798     20.956  -5.192
    ## attitudepol  -20.002      5.511  -3.630
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) gendrM
    ## genderM     -0.651       
    ## attitudepol -0.171  0.000

``` r
ranef(m4)
```

    ## $scenario
    ##   (Intercept)
    ## 1  -13.485886
    ## 2    6.741980
    ## 3   11.600172
    ## 4   20.885315
    ## 5   -1.835659
    ## 6  -12.940097
    ## 7  -10.965825
    ## 
    ## $subject
    ##    (Intercept)
    ## F1  -13.916125
    ## F2   10.425458
    ## F3    3.490667
    ## M3   28.661144
    ## M4    4.858122
    ## M7  -33.519266
    ## 
    ## with conditional variances for "scenario" "subject"
