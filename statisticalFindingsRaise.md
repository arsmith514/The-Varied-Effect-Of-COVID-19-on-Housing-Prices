---
title: "statisticalFindingsRaise.html"
output: html_document
---


### Overview:

Based on the statistical tests I performed, primarily linear regression and ANOVA tests, both described below, a few variables were statistically significant in their relationship to percent change in housing price, meaning they have a p-value less than .05.  The closer to 0 the p-value is, the less likely the results are due to random chance. With a p-value of .05, only 5% of the time will results as or more extreme as ours occur randomly. Using the ANOVA test, the variables with the smallest p-value, also known as the least likely to occur randomly, were state, median household income, and the intensity of COVID-19 restrictions. Whether a house was in a rural, urban, or suburban area was also significant in all cases. Housing price was significant only from January to Februrary and from January to March, which was probably unrelated to COVID-19. The results such as median household income, state, or rural/urban continuum can not be attributed to COVID-19 without further research. What was contrary to what I expected was the relationship between housing price and the percent change in housing price. Higher priced homes actually increased in price less than the lower priced houses. After rethinking my hypothesis, a possible explaination for the results I saw is that people who live in lower income housing in the cities want to get out of their apartments and move into houses due to the virus, leading to a high demand for lower income houses and therefore raising the price. Race did not have a significant effect on percent change in housing price. With the linear regression test I was testing numerical variables as opposed to the categorical variables above. There is strong statistical evidence between percent poverty and percent change in housing price. For my linear model, I combined May housing price, median age, percent poverty, numerical ratings for COVID-19 restrictions, state (specifically if the state is Illinois or not), and if a zip code is urban. This provided the model with every variable being at least somewhat significant in relation to both percent change in housing price and the other variables. The full model and all the statistical tests are below with R code. 

### ANOVA Test:

To figure out which variables seem to have a relationship with changes in housing price, I used two different types of statistical tests. One, a linear regression model, is built to determine how much of the change in the y variable, in this case percent change in housing price, is related to the x variable. The other is an ANOVA test. The purpose of an ANOVA test is to determine if there is a difference between multiple groups. To figure out if results are significant, a p-value is calculated. For an ANOVA test, a null hypothesis and alternative hypothesis is needed. In this case, the null hypothesis is that there is no difference between groups in regard to the change in housing price; in other words, a categorical variable has no relationship with percent change in housing price. The alternative hypothesis is that the variable does have a relationship with housing price changes. For each relevant categorical variable in my dataset, I ran 4 ANOVA tests, testing percent change in housing price from January to February, January to March, January to April, and January to May. Below is a sample of all four tests using the price of houses as the categorical variable which is preceded by the loading of the data. 


```r
library(readr)
thatRaiseData2 <- read_csv("hi/dfpt2.csv")
```

```
## Error: 'hi/dfpt2.csv' does not exist in current working directory ('/Users/abby/Desktop/App-Directory/hi').
```

```r
#categorize data by party
thatRaiseData2 <- thatRaiseData2 %>%
    mutate(party = case_when(
        `PVI#` < -4 ~ "Rep",
        `PVI#` > 4 ~ "Dem",
        TRUE ~ "purple"  )  )

#categorize data by rural/urban/suburban
thatRaiseData2 <- thatRaiseData2 %>%
    mutate(rurality = case_when(
        RUCA2 < 3.05 ~ "Urban",
        RUCA2 < 4.05 ~ "Suburban",
        RUCA2 < 4.15 ~ "Urban",
        RUCA2 < 5.05 ~ "Suburban",
        RUCA2 < 5.15 ~ "Urban",
        RUCA2 < 6.9 ~ "Suburban",
        RUCA2 < 7.05 ~ "Rural",
        RUCA2 < 7.15 ~ "Urban",
        RUCA2 < 7.25 ~ "Suburban",
        RUCA2 < 8.05 ~ "Rural",
        RUCA2 < 8.15 ~ "Urban",
        RUCA2 < 8.25 ~ "Suburban",
        RUCA2 < 10.05 ~ "Rural",
        RUCA2 < 10.15 ~ "Urban",
        RUCA2 < 10.25 ~ "Suburban",
        TRUE ~ "Rural"  )  )
```


```r
#Set percent change in housing price variables - NOTE: housing data comes out a month later, so the date actually represents data from the previous month 
perChangeMay <- ((thatRaiseData2$`2020-06-30`) - (thatRaiseData2$`2020-02-29`))/ (thatRaiseData2$`2020-02-29`) #Jan-May

perChangeApril <- ((thatRaiseData2$`2020-05-31`) - (thatRaiseData2$`2020-02-29`))/ (thatRaiseData2$`2020-02-29`) #Jan-April

perChangeMarch <- ((thatRaiseData2$`2020-04-30`) - (thatRaiseData2$`2020-02-29`))/ (thatRaiseData2$`2020-02-29`) #Jan-March

perChangeFeb <- ((thatRaiseData2$`2020-03-31`) - (thatRaiseData2$`2020-02-29`))/ (thatRaiseData2$`2020-02-29`) #Jan-Feb
```

January to Feburary:

```r
   summary(aov(perChangeFeb ~ HousingCategorical, data = thatRaiseData2))
```

```
##                     Df    Sum Sq
## HousingCategorical   3 0.0001378
## Residuals          191 0.0026623
##                      Mean Sq
## HousingCategorical 4.592e-05
## Residuals          1.394e-05
##                    F value Pr(>F)
## HousingCategorical   3.294 0.0217
## Residuals                        
##                     
## HousingCategorical *
## Residuals           
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

January to March:

```r
   summary(aov(perChangeMarch ~ HousingCategorical, data = thatRaiseData2))
```

```
##                     Df   Sum Sq
## HousingCategorical   3 0.000416
## Residuals          191 0.009530
##                      Mean Sq
## HousingCategorical 1.388e-04
## Residuals          4.989e-05
##                    F value Pr(>F)
## HousingCategorical   2.781 0.0423
## Residuals                        
##                     
## HousingCategorical *
## Residuals           
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

January to April:

```r
   summary(aov(perChangeApril ~ HousingCategorical, data = thatRaiseData2))
```

```
##                     Df   Sum Sq
## HousingCategorical   3 0.000762
## Residuals          191 0.020669
##                      Mean Sq
## HousingCategorical 0.0002540
## Residuals          0.0001082
##                    F value Pr(>F)
## HousingCategorical   2.347  0.074
## Residuals                        
##                     
## HousingCategorical .
## Residuals           
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

January to May:

```r
   summary(aov(perChangeMay ~ HousingCategorical, data = thatRaiseData2))
```

```
##                     Df  Sum Sq
## HousingCategorical   3 0.00086
## Residuals          191 0.03698
##                      Mean Sq
## HousingCategorical 0.0002868
## Residuals          0.0001936
##                    F value Pr(>F)
## HousingCategorical   1.481  0.221
## Residuals
```
                        
The p-value is shown under PR(>F), and is generally considered significant if its value is under .05. This means that if there is no relationship between a house’s price and the percent change in housing price, five percent of the time we will reject the null hypothesis and conclude there is actually a relationship. In the results above, we see that between January and May, with a p-value of 0.221, we do not see evidence of a relationship between the price of a house and ther change in price. In January to April, we get a p-value of .074, which is still not significant. But, from January to March there is a p-value of .0423, and from January to February there is a p-value of .0217. While both of those are significant, it seems unlikely that COVID-19 was related to that, as the effects of the virus were not large in February, and certainly not as great as during March or April.

### Linear Regression: 

The linear regression models use a straight line to help determine the relationship between numerical variables. A negative slope indicates that there is a negative correlation between the variables, and a larger magnitude of the slope indicates that for every one unit increase in x, there is a larger increase in y. The magnitude of the slope has no relationship to the correlation between the x and y variables, meaning a slope of 100 is not necessarily more statistically significant than a slope of .01. When using RStudio to create a linear regression, I create two plots; one shows the data points with the regression line, and the other shows the residuals. Residuals are the difference between a predicted value for a point, based on the regression line, and the actual value the data found. In an ideal linear regression situation, the residuals will appear random. After running a linear regression, I get a correlation constant which tells how much of the variability seems to be explainable by the relationship between the two variables. There also is a p-value that is used to determine if the results are significant. That is the p-value for the slope, the second value under PR(>|t|). As with the ANOVA test, a significant p-value does not indicate that the x axis causes a change in the y variable, nor does it guarantee a relationship, as the results could be due to chance. 

Linear Regression Graph for Percent Poverty vs Percent Change in Housing Price:

```r
  ggplot(thatRaiseData2, aes(x = as.numeric(PercentPoverty), y = perChangeMay)) + 
        geom_point() + #alpha changes opaci
        geom_smooth(method = "lm")
```

<img src="figure/unnamed-chunk-7-1.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" style="display: block; margin: auto;" />

Residuals Plot:

```r
  ggplot(lm(as.numeric(PercentPoverty)~perChangeMay, data=thatRaiseData2)) + 
        geom_point(aes(x=.fitted, y=.resid))
```

<img src="figure/unnamed-chunk-8-1.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" style="display: block; margin: auto;" />

Linear Regression Summary

```r
      summary(lm(perChangeMay ~ as.numeric(PercentPoverty), data = thatRaiseData2))
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(PercentPoverty), data = thatRaiseData2)
## 
## Residuals:
##       Min        1Q    Median 
## -0.026828 -0.007214 -0.000607 
##        3Q       Max 
##  0.006027  0.078217 
## 
## Coefficients:
##                             Estimate
## (Intercept)                0.0081855
## as.numeric(PercentPoverty) 0.0003532
##                            Std. Error
## (Intercept)                 0.0015961
## as.numeric(PercentPoverty)  0.0001031
##                            t value
## (Intercept)                  5.129
## as.numeric(PercentPoverty)   3.427
##                            Pr(>|t|)
## (Intercept)                7.23e-07
## as.numeric(PercentPoverty)  0.00075
##                               
## (Intercept)                ***
## as.numeric(PercentPoverty) ***
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01361 on 188 degrees of freedom
##   (5 observations deleted due to missingness)
## Multiple R-squared:  0.05879,	Adjusted R-squared:  0.05378 
## F-statistic: 11.74 on 1 and 188 DF,  p-value: 0.0007504
```

```r
      cor(perChangeMay, as.numeric(thatRaiseData2$PercentPoverty), use="complete.obs")
```

```
## [1] 0.2424607
```

Since the p-value is 0.00075, which is much smaller than .05, there appears to be strong evidence for a significant relationship between percent poverty and the change in housing price. Since the correlation is only 0.2424607, though, only about 24% of the variability in percent change in housing price appears to be explained by percent poverty. 

### My Model:

In an attempt to create a linear model using more than one variable, I made a linear regression using all relevant values. The goal was to predict the y variable, percent change in housing price from January to May, using different x variables. The p-values given for each slope tells how significant a variable is given all the other variables. After the original model is displayed, I can get rid of any variables that seem to have little to no impact on the model. With several repetitions of creating a model and deleting variables that are the least impactful, I ended with a model in which 7 variables, some numerical and some categorical, are significant and helpful in predicting the percent change in housing price. Two of those models are shown in progression.

Model 1:

```r
summary(lm(perChangeMay ~ as.numeric(`2020-06-30`) + as.numeric(PercentHispanic) + as.numeric(MedianAge) + as.numeric(PercentPoverty) + as.numeric(NumericalRestriction) + party + State + rurality, data = thatRaiseData2))
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(`2020-06-30`) + as.numeric(PercentHispanic) + 
##     as.numeric(MedianAge) + as.numeric(PercentPoverty) + as.numeric(NumericalRestriction) + 
##     party + State + rurality, data = thatRaiseData2)
## 
## Residuals:
##       Min        1Q    Median 
## -0.025116 -0.006510  0.000031 
##        3Q       Max 
##  0.005134  0.065561 
## 
## Coefficients: (1 not defined because of singularities)
##                                    Estimate
## (Intercept)                      -3.324e-02
## as.numeric(`2020-06-30`)         -6.707e-09
## as.numeric(PercentHispanic)      -1.384e-04
## as.numeric(MedianAge)            -3.876e-04
## as.numeric(PercentPoverty)        2.174e-04
## as.numeric(NumericalRestriction)  1.552e-03
## partypurple                       3.994e-03
## partyRep                          4.799e-03
## StateFL                          -1.311e-02
## StateIL                          -1.083e-02
## StateMD                           2.377e-03
## StateNM                           2.560e-02
## StatePA                          -3.515e-03
## StateSC                          -5.087e-02
## StateVA                                  NA
## ruralitySuburban                 -6.662e-03
## ruralityUrban                     6.178e-03
##                                  Std. Error
## (Intercept)                       3.184e-02
## as.numeric(`2020-06-30`)          4.305e-09
## as.numeric(PercentHispanic)       6.347e-05
## as.numeric(MedianAge)             1.638e-04
## as.numeric(PercentPoverty)        1.262e-04
## as.numeric(NumericalRestriction)  8.909e-04
## partypurple                       2.930e-03
## partyRep                          2.340e-03
## StateFL                           1.151e-02
## StateIL                           3.072e-03
## StateMD                           5.356e-03
## StateNM                           1.136e-02
## StatePA                           3.649e-03
## StateSC                           3.179e-02
## StateVA                                  NA
## ruralitySuburban                  4.375e-03
## ruralityUrban                     3.735e-03
##                                  t value
## (Intercept)                       -1.044
## as.numeric(`2020-06-30`)          -1.558
## as.numeric(PercentHispanic)       -2.181
## as.numeric(MedianAge)             -2.366
## as.numeric(PercentPoverty)         1.723
## as.numeric(NumericalRestriction)   1.743
## partypurple                        1.363
## partyRep                           2.051
## StateFL                           -1.138
## StateIL                           -3.527
## StateMD                            0.444
## StateNM                            2.254
## StatePA                           -0.963
## StateSC                           -1.600
## StateVA                               NA
## ruralitySuburban                  -1.523
## ruralityUrban                      1.654
##                                  Pr(>|t|)
## (Intercept)                      0.297852
## as.numeric(`2020-06-30`)         0.121060
## as.numeric(PercentHispanic)      0.030512
## as.numeric(MedianAge)            0.019062
## as.numeric(PercentPoverty)       0.086752
## as.numeric(NumericalRestriction) 0.083163
## partypurple                      0.174650
## partyRep                         0.041768
## StateFL                          0.256540
## StateIL                          0.000538
## StateMD                          0.657810
## StateNM                          0.025476
## StatePA                          0.336749
## StateSC                          0.111345
## StateVA                                NA
## ruralitySuburban                 0.129652
## ruralityUrban                    0.099872
##                                     
## (Intercept)                         
## as.numeric(`2020-06-30`)            
## as.numeric(PercentHispanic)      *  
## as.numeric(MedianAge)            *  
## as.numeric(PercentPoverty)       .  
## as.numeric(NumericalRestriction) .  
## partypurple                         
## partyRep                         *  
## StateFL                             
## StateIL                          ***
## StateMD                             
## StateNM                          *  
## StatePA                             
## StateSC                             
## StateVA                             
## ruralitySuburban                    
## ruralityUrban                    .  
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01197 on 174 degrees of freedom
##   (5 observations deleted due to missingness)
## Multiple R-squared:  0.3267,	Adjusted R-squared:  0.2687 
## F-statistic: 5.629 on 15 and 174 DF,  p-value: 2.625e-09
```
Model 2:

```r
summary(lm(perChangeMay ~ as.numeric(`2020-06-30`) + as.numeric(MedianAge) + as.numeric(PercentPoverty) + as.numeric(NumericalRestriction) + party + (State == "IL") + (rurality == "Urban"), data = thatRaiseData2))
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(`2020-06-30`) + as.numeric(MedianAge) + 
##     as.numeric(PercentPoverty) + as.numeric(NumericalRestriction) + 
##     party + (State == "IL") + (rurality == "Urban"), data = thatRaiseData2)
## 
## Residuals:
##       Min        1Q    Median 
## -0.026663 -0.006444 -0.000103 
##        3Q       Max 
##  0.005159  0.066036 
## 
## Coefficients:
##                                    Estimate
## (Intercept)                       3.574e-03
## as.numeric(`2020-06-30`)         -7.032e-09
## as.numeric(MedianAge)            -1.767e-04
## as.numeric(PercentPoverty)        2.849e-04
## as.numeric(NumericalRestriction)  1.578e-04
## partypurple                       4.000e-03
## partyRep                          4.427e-03
## State == "IL"TRUE                -1.001e-02
## rurality == "Urban"TRUE           9.634e-03
##                                  Std. Error
## (Intercept)                       7.645e-03
## as.numeric(`2020-06-30`)          4.251e-09
## as.numeric(MedianAge)             1.466e-04
## as.numeric(PercentPoverty)        1.174e-04
## as.numeric(NumericalRestriction)  8.140e-05
## partypurple                       2.682e-03
## partyRep                          2.147e-03
## State == "IL"TRUE                 2.345e-03
## rurality == "Urban"TRUE           2.537e-03
##                                  t value
## (Intercept)                        0.467
## as.numeric(`2020-06-30`)          -1.654
## as.numeric(MedianAge)             -1.205
## as.numeric(PercentPoverty)         2.426
## as.numeric(NumericalRestriction)   1.939
## partypurple                        1.491
## partyRep                           2.062
## State == "IL"TRUE                 -4.268
## rurality == "Urban"TRUE            3.797
##                                  Pr(>|t|)
## (Intercept)                        0.6407
## as.numeric(`2020-06-30`)           0.0998
## as.numeric(MedianAge)              0.2297
## as.numeric(PercentPoverty)         0.0163
## as.numeric(NumericalRestriction)   0.0541
## partypurple                        0.1376
## partyRep                           0.0406
## State == "IL"TRUE                3.17e-05
## rurality == "Urban"TRUE            0.0002
##                                     
## (Intercept)                         
## as.numeric(`2020-06-30`)         .  
## as.numeric(MedianAge)               
## as.numeric(PercentPoverty)       *  
## as.numeric(NumericalRestriction) .  
## partypurple                         
## partyRep                         *  
## State == "IL"TRUE                ***
## rurality == "Urban"TRUE          ***
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01218 on 181 degrees of freedom
##   (5 observations deleted due to missingness)
## Multiple R-squared:  0.2743,	Adjusted R-squared:  0.2422 
## F-statistic: 8.552 on 8 and 181 DF,  p-value: 7.442e-10
```

### My Code


Race:


```r
summary(aov(perChangeMay ~ Race, data = thatRaiseData2))
```

```
##              Df  Sum Sq   Mean Sq
## Race          3 0.00102 0.0003404
## Residuals   191 0.03682 0.0001928
##             F value Pr(>F)
## Race          1.766  0.155
## Residuals
```

```r
summary(aov(perChangeApril ~ Race, data = thatRaiseData2))
```

```
##              Df   Sum Sq   Mean Sq
## Race          3 0.000735 0.0002449
## Residuals   191 0.020696 0.0001084
##             F value Pr(>F)  
## Race           2.26 0.0828 .
## Residuals                   
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(perChangeMarch ~ Race, data = thatRaiseData2))
```

```
##              Df   Sum Sq   Mean Sq
## Race          3 0.000358 0.0001194
## Residuals   191 0.009588 0.0000502
##             F value Pr(>F)  
## Race          2.378 0.0712 .
## Residuals                   
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(perChangeFeb ~ Race, data = thatRaiseData2))
```

```
##              Df    Sum Sq
## Race          3 0.0000946
## Residuals   191 0.0027055
##               Mean Sq F value
## Race        3.154e-05   2.226
## Residuals   1.417e-05        
##             Pr(>F)  
## Race        0.0865 .
## Residuals           
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```


Party: 


```r
summary(aov(perChangeMay ~ party, data = thatRaiseData2))
```

```
##              Df  Sum Sq   Mean Sq
## party         2 0.00035 0.0001758
## Residuals   192 0.03749 0.0001953
##             F value Pr(>F)
## party           0.9  0.408
## Residuals
```

```r
summary(aov(perChangeApril ~ party, data = thatRaiseData2))
```

```
##              Df   Sum Sq   Mean Sq
## party         2 0.000302 0.0001508
## Residuals   192 0.021129 0.0001101
##             F value Pr(>F)
## party          1.37  0.257
## Residuals
```

```r
summary(aov(perChangeMarch ~ party, data = thatRaiseData2))
```

```
##              Df   Sum Sq   Mean Sq
## party         2 0.000159 7.943e-05
## Residuals   192 0.009787 5.098e-05
##             F value Pr(>F)
## party         1.558  0.213
## Residuals
```

```r
summary(aov(perChangeFeb ~ party, data = thatRaiseData2))
```

```
##              Df   Sum Sq   Mean Sq
## party         2 5.03e-05 2.515e-05
## Residuals   192 2.75e-03 1.432e-05
##             F value Pr(>F)
## party         1.756  0.175
## Residuals
```

Housing Categorical:


```r
summary(aov(perChangeMay ~ HousingCategorical, data = thatRaiseData2))
```

```
##                     Df  Sum Sq
## HousingCategorical   3 0.00086
## Residuals          191 0.03698
##                      Mean Sq
## HousingCategorical 0.0002868
## Residuals          0.0001936
##                    F value Pr(>F)
## HousingCategorical   1.481  0.221
## Residuals
```

```r
summary(aov(perChangeApril ~ HousingCategorical, data = thatRaiseData2))
```

```
##                     Df   Sum Sq
## HousingCategorical   3 0.000762
## Residuals          191 0.020669
##                      Mean Sq
## HousingCategorical 0.0002540
## Residuals          0.0001082
##                    F value Pr(>F)
## HousingCategorical   2.347  0.074
## Residuals                        
##                     
## HousingCategorical .
## Residuals           
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(perChangeMarch ~ HousingCategorical, data = thatRaiseData2))
```

```
##                     Df   Sum Sq
## HousingCategorical   3 0.000416
## Residuals          191 0.009530
##                      Mean Sq
## HousingCategorical 1.388e-04
## Residuals          4.989e-05
##                    F value Pr(>F)
## HousingCategorical   2.781 0.0423
## Residuals                        
##                     
## HousingCategorical *
## Residuals           
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(perChangeFeb ~ HousingCategorical, data = thatRaiseData2))
```

```
##                     Df    Sum Sq
## HousingCategorical   3 0.0001378
## Residuals          191 0.0026623
##                      Mean Sq
## HousingCategorical 4.592e-05
## Residuals          1.394e-05
##                    F value Pr(>F)
## HousingCategorical   3.294 0.0217
## Residuals                        
##                     
## HousingCategorical *
## Residuals           
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

State:


```r
summary(aov(perChangeMay ~ State, data = thatRaiseData2))
```

```
##              Df  Sum Sq   Mean Sq
## State         7 0.00629 0.0008985
## Residuals   187 0.03155 0.0001687
##             F value   Pr(>F)    
## State         5.325 1.43e-05 ***
## Residuals                       
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(perChangeApril ~ State, data = thatRaiseData2))
```

```
##              Df   Sum Sq   Mean Sq
## State         7 0.003233 0.0004619
## Residuals   187 0.018197 0.0000973
##             F value   Pr(>F)    
## State         4.747 6.26e-05 ***
## Residuals                       
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(perChangeMarch ~ State, data = thatRaiseData2))
```

```
##              Df   Sum Sq   Mean Sq
## State         7 0.001404 2.006e-04
## Residuals   187 0.008542 4.568e-05
##             F value   Pr(>F)    
## State         4.392 0.000155 ***
## Residuals                       
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(perChangeFeb ~ State, data = thatRaiseData2))
```

```
##              Df    Sum Sq
## State         7 0.0004029
## Residuals   187 0.0023972
##               Mean Sq F value
## State       5.756e-05    4.49
## Residuals   1.282e-05        
##               Pr(>F)    
## State       0.000121 ***
## Residuals               
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

Rurality:


```r
summary(aov(perChangeMay ~ rurality, data = thatRaiseData2))
```

```
##              Df  Sum Sq   Mean Sq
## rurality      2 0.00211 0.0010565
## Residuals   192 0.03573 0.0001861
##             F value  Pr(>F)   
## rurality      5.677 0.00402 **
## Residuals                     
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(perChangeApril ~ rurality, data = thatRaiseData2))
```

```
##              Df   Sum Sq   Mean Sq
## rurality      2 0.001087 0.0005433
## Residuals   192 0.020344 0.0001060
##             F value  Pr(>F)   
## rurality      5.128 0.00677 **
## Residuals                     
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(perChangeMarch ~ rurality, data = thatRaiseData2))
```

```
##              Df   Sum Sq   Mean Sq
## rurality      2 0.000400 1.999e-04
## Residuals   192 0.009546 4.972e-05
##             F value Pr(>F)  
## rurality      4.021 0.0195 *
## Residuals                   
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(perChangeFeb ~ rurality, data = thatRaiseData2))
```

```
##              Df    Sum Sq
## rurality      2 0.0001052
## Residuals   192 0.0026949
##               Mean Sq F value
## rurality    5.258e-05   3.746
## Residuals   1.404e-05        
##             Pr(>F)  
## rurality    0.0254 *
## Residuals           
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

Median Household Income:


```r
summary(aov(perChangeMay ~ MedHouseIncomeCategorical, data = thatRaiseData2))
```

```
##                            Df
## MedHouseIncomeCategorical   4
## Residuals                 185
##                            Sum Sq
## MedHouseIncomeCategorical 0.00249
## Residuals                 0.03451
##                             Mean Sq
## MedHouseIncomeCategorical 0.0006230
## Residuals                 0.0001865
##                           F value
## MedHouseIncomeCategorical    3.34
## Residuals                        
##                           Pr(>F)  
## MedHouseIncomeCategorical 0.0114 *
## Residuals                         
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 5 observations deleted due to missingness
```

```r
summary(aov(perChangeApril ~ MedHouseIncomeCategorical, data = thatRaiseData2))
```

```
##                            Df
## MedHouseIncomeCategorical   4
## Residuals                 185
##                             Sum Sq
## MedHouseIncomeCategorical 0.001842
## Residuals                 0.019045
##                             Mean Sq
## MedHouseIncomeCategorical 0.0004604
## Residuals                 0.0001029
##                           F value
## MedHouseIncomeCategorical   4.473
## Residuals                        
##                            Pr(>F)
## MedHouseIncomeCategorical 0.00179
## Residuals                        
##                             
## MedHouseIncomeCategorical **
## Residuals                   
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 5 observations deleted due to missingness
```

```r
summary(aov(perChangeMarch ~ MedHouseIncomeCategorical, data = thatRaiseData2))
```

```
##                            Df
## MedHouseIncomeCategorical   4
## Residuals                 185
##                             Sum Sq
## MedHouseIncomeCategorical 0.000947
## Residuals                 0.008798
##                             Mean Sq
## MedHouseIncomeCategorical 2.368e-04
## Residuals                 4.756e-05
##                           F value
## MedHouseIncomeCategorical   4.979
## Residuals                        
##                            Pr(>F)
## MedHouseIncomeCategorical 0.00078
## Residuals                        
##                              
## MedHouseIncomeCategorical ***
## Residuals                    
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 5 observations deleted due to missingness
```

```r
summary(aov(perChangeFeb ~ MedHouseIncomeCategorical, data = thatRaiseData2))
```

```
##                            Df
## MedHouseIncomeCategorical   4
## Residuals                 185
##                              Sum Sq
## MedHouseIncomeCategorical 0.0002952
## Residuals                 0.0024581
##                             Mean Sq
## MedHouseIncomeCategorical 7.380e-05
## Residuals                 1.329e-05
##                           F value
## MedHouseIncomeCategorical   5.554
## Residuals                        
##                             Pr(>F)
## MedHouseIncomeCategorical 0.000304
## Residuals                         
##                              
## MedHouseIncomeCategorical ***
## Residuals                    
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 5 observations deleted due to missingness
```
 
Covid Restrictions:


```r
summary(aov(perChangeMay ~ CovidRestrictionsCategorical, data = thatRaiseData2))
```

```
##                               Df
## CovidRestrictionsCategorical   2
## Residuals                    192
##                               Sum Sq
## CovidRestrictionsCategorical 0.00332
## Residuals                    0.03452
##                                Mean Sq
## CovidRestrictionsCategorical 0.0016598
## Residuals                    0.0001798
##                              F value
## CovidRestrictionsCategorical   9.231
## Residuals                           
##                                Pr(>F)
## CovidRestrictionsCategorical 0.000149
## Residuals                            
##                                 
## CovidRestrictionsCategorical ***
## Residuals                       
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(perChangeApril ~ CovidRestrictionsCategorical, data = thatRaiseData2))
```

```
##                               Df
## CovidRestrictionsCategorical   2
## Residuals                    192
##                                Sum Sq
## CovidRestrictionsCategorical 0.001862
## Residuals                    0.019569
##                                Mean Sq
## CovidRestrictionsCategorical 0.0009309
## Residuals                    0.0001019
##                              F value
## CovidRestrictionsCategorical   9.134
## Residuals                           
##                                Pr(>F)
## CovidRestrictionsCategorical 0.000162
## Residuals                            
##                                 
## CovidRestrictionsCategorical ***
## Residuals                       
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(perChangeMarch ~ CovidRestrictionsCategorical, data = thatRaiseData2))
```

```
##                               Df
## CovidRestrictionsCategorical   2
## Residuals                    192
##                                Sum Sq
## CovidRestrictionsCategorical 0.000830
## Residuals                    0.009116
##                                Mean Sq
## CovidRestrictionsCategorical 0.0004151
## Residuals                    0.0000475
##                              F value
## CovidRestrictionsCategorical   8.743
## Residuals                           
##                                Pr(>F)
## CovidRestrictionsCategorical 0.000232
## Residuals                            
##                                 
## CovidRestrictionsCategorical ***
## Residuals                       
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

```r
summary(aov(perChangeFeb ~ CovidRestrictionsCategorical, data = thatRaiseData2))
```

```
##                               Df
## CovidRestrictionsCategorical   2
## Residuals                    192
##                                 Sum Sq
## CovidRestrictionsCategorical 0.0002741
## Residuals                    0.0025259
##                                Mean Sq
## CovidRestrictionsCategorical 1.371e-04
## Residuals                    1.316e-05
##                              F value
## CovidRestrictionsCategorical   10.42
## Residuals                           
##                                Pr(>F)
## CovidRestrictionsCategorical 5.06e-05
## Residuals                            
##                                 
## CovidRestrictionsCategorical ***
## Residuals                       
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
```

Covid Impact:


```r
summary(aov(perChangeMay ~ CovidImpactCategorical, data = thatRaiseData2))
```

```
##                         Df  Sum Sq
## CovidImpactCategorical   3 0.00044
## Residuals              185 0.03616
##                          Mean Sq
## CovidImpactCategorical 0.0001467
## Residuals              0.0001954
##                        F value
## CovidImpactCategorical   0.751
## Residuals                     
##                        Pr(>F)
## CovidImpactCategorical  0.523
## Residuals                    
## 6 observations deleted due to missingness
```

```r
summary(aov(perChangeApril ~ CovidImpactCategorical, data = thatRaiseData2))
```

```
##                         Df
## CovidImpactCategorical   3
## Residuals              185
##                          Sum Sq
## CovidImpactCategorical 0.000206
## Residuals              0.020317
##                          Mean Sq
## CovidImpactCategorical 6.868e-05
## Residuals              1.098e-04
##                        F value
## CovidImpactCategorical   0.625
## Residuals                     
##                        Pr(>F)
## CovidImpactCategorical  0.599
## Residuals                    
## 6 observations deleted due to missingness
```

```r
summary(aov(perChangeMarch ~ CovidImpactCategorical, data = thatRaiseData2))
```

```
##                         Df
## CovidImpactCategorical   3
## Residuals              185
##                          Sum Sq
## CovidImpactCategorical 0.000126
## Residuals              0.009385
##                          Mean Sq
## CovidImpactCategorical 4.202e-05
## Residuals              5.073e-05
##                        F value
## CovidImpactCategorical   0.828
## Residuals                     
##                        Pr(>F)
## CovidImpactCategorical   0.48
## Residuals                    
## 6 observations deleted due to missingness
```

```r
summary(aov(perChangeFeb ~ CovidImpactCategorical, data = thatRaiseData2))
```

```
##                         Df
## CovidImpactCategorical   3
## Residuals              185
##                          Sum Sq
## CovidImpactCategorical 6.88e-05
## Residuals              2.58e-03
##                          Mean Sq
## CovidImpactCategorical 2.294e-05
## Residuals              1.394e-05
##                        F value
## CovidImpactCategorical   1.645
## Residuals                     
##                        Pr(>F)
## CovidImpactCategorical   0.18
## Residuals                    
## 6 observations deleted due to missingness
```








Other Stuff:
Yes:
Believe it:

Percent Poverty:

```r
ggplot(thatRaiseData2, aes(x = as.numeric(PercentPoverty), y = perChangeMay)) + 
  geom_point() + #alpha changes opaci
  geom_smooth(method = "lm")
```

<img src="figure/unnamed-chunk-20-1.png" title="plot of chunk unnamed-chunk-20" alt="plot of chunk unnamed-chunk-20" style="display: block; margin: auto;" />

```r
ggplot(lm(as.numeric(PercentPoverty)~perChangeMay, data=thatRaiseData2)) + 
  geom_point(aes(x=.fitted, y=.resid))
```

<img src="figure/unnamed-chunk-20-2.png" title="plot of chunk unnamed-chunk-20" alt="plot of chunk unnamed-chunk-20" style="display: block; margin: auto;" />

```r
lm(perChangeMay ~ as.numeric(PercentPoverty), data = thatRaiseData2)
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(PercentPoverty), data = thatRaiseData2)
## 
## Coefficients:
##                (Intercept)  
##                  0.0081855  
## as.numeric(PercentPoverty)  
##                  0.0003532
```

```r
summary(lm(perChangeMay ~ as.numeric(PercentPoverty), data = thatRaiseData2))
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(PercentPoverty), data = thatRaiseData2)
## 
## Residuals:
##       Min        1Q    Median 
## -0.026828 -0.007214 -0.000607 
##        3Q       Max 
##  0.006027  0.078217 
## 
## Coefficients:
##                             Estimate
## (Intercept)                0.0081855
## as.numeric(PercentPoverty) 0.0003532
##                            Std. Error
## (Intercept)                 0.0015961
## as.numeric(PercentPoverty)  0.0001031
##                            t value
## (Intercept)                  5.129
## as.numeric(PercentPoverty)   3.427
##                            Pr(>|t|)
## (Intercept)                7.23e-07
## as.numeric(PercentPoverty)  0.00075
##                               
## (Intercept)                ***
## as.numeric(PercentPoverty) ***
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01361 on 188 degrees of freedom
##   (5 observations deleted due to missingness)
## Multiple R-squared:  0.05879,	Adjusted R-squared:  0.05378 
## F-statistic: 11.74 on 1 and 188 DF,  p-value: 0.0007504
```

```r
cor(perChangeMay, as.numeric(thatRaiseData2$PercentPoverty), use="complete.obs")
```

```
## [1] 0.2424607
```

Median Age

```r
ggplot(thatRaiseData2, aes(x = as.numeric(MedianAge), y = perChangeMay)) + 
  geom_point() + #alpha changes opaci
  geom_smooth(method = "lm")
```

<img src="figure/unnamed-chunk-21-1.png" title="plot of chunk unnamed-chunk-21" alt="plot of chunk unnamed-chunk-21" style="display: block; margin: auto;" />

```r
ggplot(lm(as.numeric(MedianAge)~perChangeMay, data=thatRaiseData2)) + 
  geom_point(aes(x=.fitted, y=.resid))
```

<img src="figure/unnamed-chunk-21-2.png" title="plot of chunk unnamed-chunk-21" alt="plot of chunk unnamed-chunk-21" style="display: block; margin: auto;" />

```r
lm(perChangeMay ~ as.numeric(MedianAge), data = thatRaiseData2)
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(MedianAge), data = thatRaiseData2)
## 
## Coefficients:
##           (Intercept)  
##             0.0220076  
## as.numeric(MedianAge)  
##            -0.0002318
```

```r
summary(lm(perChangeMay ~ as.numeric(MedianAge), data = thatRaiseData2))
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(MedianAge), data = thatRaiseData2)
## 
## Residuals:
##       Min        1Q    Median 
## -0.025906 -0.008534 -0.000530 
##        3Q       Max 
##  0.005933  0.078758 
## 
## Coefficients:
##                         Estimate
## (Intercept)            0.0220076
## as.numeric(MedianAge) -0.0002318
##                       Std. Error
## (Intercept)            0.0056917
## as.numeric(MedianAge)  0.0001344
##                       t value
## (Intercept)             3.867
## as.numeric(MedianAge)  -1.724
##                       Pr(>|t|)    
## (Intercept)           0.000151 ***
## as.numeric(MedianAge) 0.086278 .  
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0139 on 193 degrees of freedom
## Multiple R-squared:  0.01517,	Adjusted R-squared:  0.01007 
## F-statistic: 2.973 on 1 and 193 DF,  p-value: 0.08628
```

```r
cor(perChangeMay, as.numeric(thatRaiseData2$MedianAge), use="complete.obs")
```

```
## [1] -0.1231636
```

Covid Per 10,000

```r
ggplot(thatRaiseData2, aes(x = as.numeric(Covid.per.10.000), y = perChangeMay)) + 
  geom_point() + #alpha changes opaci
  geom_smooth(method = "lm")
```

<img src="figure/unnamed-chunk-22-1.png" title="plot of chunk unnamed-chunk-22" alt="plot of chunk unnamed-chunk-22" style="display: block; margin: auto;" />

```r
ggplot(lm(as.numeric(Covid.per.10.000)~perChangeMay, data=thatRaiseData2)) + 
  geom_point(aes(x=.fitted, y=.resid))
```

<img src="figure/unnamed-chunk-22-2.png" title="plot of chunk unnamed-chunk-22" alt="plot of chunk unnamed-chunk-22" style="display: block; margin: auto;" />

```r
lm(perChangeMay ~ as.numeric(Covid.per.10.000), data = thatRaiseData2)
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(Covid.per.10.000), data = thatRaiseData2)
## 
## Coefficients:
##                  (Intercept)  
##                    1.238e-02  
## as.numeric(Covid.per.10.000)  
##                   -7.923e-07
```

```r
summary(lm(perChangeMay ~ as.numeric(Covid.per.10.000), data = thatRaiseData2))
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(Covid.per.10.000), data = thatRaiseData2)
## 
## Residuals:
##       Min        1Q    Median 
## -0.025266 -0.008414 -0.001043 
##        3Q       Max 
##  0.005608  0.080304 
## 
## Coefficients:
##                                Estimate
## (Intercept)                   1.238e-02
## as.numeric(Covid.per.10.000) -7.923e-07
##                              Std. Error
## (Intercept)                   1.347e-03
## as.numeric(Covid.per.10.000)  1.261e-05
##                              t value
## (Intercept)                    9.191
## as.numeric(Covid.per.10.000)  -0.063
##                              Pr(>|t|)
## (Intercept)                    <2e-16
## as.numeric(Covid.per.10.000)     0.95
##                                 
## (Intercept)                  ***
## as.numeric(Covid.per.10.000)    
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01399 on 187 degrees of freedom
##   (6 observations deleted due to missingness)
## Multiple R-squared:  2.111e-05,	Adjusted R-squared:  -0.005326 
## F-statistic: 0.003947 on 1 and 187 DF,  p-value: 0.95
```

```r
cor(perChangeMay, as.numeric(thatRaiseData2$Covid.per.10.000), use="complete.obs")
```

```
## [1] -0.004594322
```

PVI:

```r
ggplot(thatRaiseData2, aes(x = as.numeric(`PVI#`), y = perChangeMay)) + 
  geom_point() + #alpha changes opaci
  geom_smooth(method = "lm")
```

<img src="figure/unnamed-chunk-23-1.png" title="plot of chunk unnamed-chunk-23" alt="plot of chunk unnamed-chunk-23" style="display: block; margin: auto;" />

```r
ggplot(lm(as.numeric(`PVI#`)~perChangeMay, data=thatRaiseData2)) + 
  geom_point(aes(x=.fitted, y=.resid))
```

<img src="figure/unnamed-chunk-23-2.png" title="plot of chunk unnamed-chunk-23" alt="plot of chunk unnamed-chunk-23" style="display: block; margin: auto;" />

```r
lm(perChangeMay ~ as.numeric(`PVI#`), data = thatRaiseData2)
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(`PVI#`), data = thatRaiseData2)
## 
## Coefficients:
##        (Intercept)  
##          1.249e-02  
## as.numeric(`PVI#`)  
##         -9.097e-05
```

```r
summary(lm(perChangeMay ~ as.numeric(`PVI#`), data = thatRaiseData2))
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(`PVI#`), data = thatRaiseData2)
## 
## Residuals:
##       Min        1Q    Median 
## -0.024516 -0.007894 -0.001109 
##        3Q       Max 
##  0.006255  0.079346 
## 
## Coefficients:
##                      Estimate
## (Intercept)         1.249e-02
## as.numeric(`PVI#`) -9.097e-05
##                    Std. Error
## (Intercept)         1.007e-03
## as.numeric(`PVI#`)  7.786e-05
##                    t value
## (Intercept)         12.405
## as.numeric(`PVI#`)  -1.168
##                    Pr(>|t|)    
## (Intercept)          <2e-16 ***
## as.numeric(`PVI#`)    0.244    
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01395 on 193 degrees of freedom
## Multiple R-squared:  0.007024,	Adjusted R-squared:  0.001879 
## F-statistic: 1.365 on 1 and 193 DF,  p-value: 0.2441
```

```r
cor(perChangeMay, as.numeric(thatRaiseData2$`PVI#`), use="complete.obs")
```

```
## [1] -0.08380735
```

Population Density:

```r
ggplot(thatRaiseData2, aes(x = as.numeric(PopDensity), y = perChangeMay)) + 
  geom_point() + #alpha changes opaci
  geom_smooth(method = "lm")
```

<img src="figure/unnamed-chunk-24-1.png" title="plot of chunk unnamed-chunk-24" alt="plot of chunk unnamed-chunk-24" style="display: block; margin: auto;" />

```r
ggplot(lm(as.numeric(PopDensity)~perChangeMay, data=thatRaiseData2)) + 
  geom_point(aes(x=.fitted, y=.resid))
```

<img src="figure/unnamed-chunk-24-2.png" title="plot of chunk unnamed-chunk-24" alt="plot of chunk unnamed-chunk-24" style="display: block; margin: auto;" />

```r
lm(perChangeMay ~ as.numeric(PopDensity), data = thatRaiseData2)
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(PopDensity), data = thatRaiseData2)
## 
## Coefficients:
##            (Intercept)  
##              1.259e-02  
## as.numeric(PopDensity)  
##             -1.234e-07
```

```r
summary(lm(perChangeMay ~ as.numeric(PopDensity), data = thatRaiseData2))
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(PopDensity), data = thatRaiseData2)
## 
## Residuals:
##       Min        1Q    Median 
## -0.025106 -0.008520 -0.001229 
##        3Q       Max 
##  0.006485  0.080125 
## 
## Coefficients:
##                          Estimate
## (Intercept)             1.259e-02
## as.numeric(PopDensity) -1.234e-07
##                        Std. Error
## (Intercept)             1.238e-03
## as.numeric(PopDensity)  3.715e-07
##                        t value
## (Intercept)             10.170
## as.numeric(PopDensity)  -0.332
##                        Pr(>|t|)
## (Intercept)              <2e-16
## as.numeric(PopDensity)     0.74
##                           
## (Intercept)            ***
## as.numeric(PopDensity)    
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.014 on 193 degrees of freedom
## Multiple R-squared:  0.0005715,	Adjusted R-squared:  -0.004607 
## F-statistic: 0.1104 on 1 and 193 DF,  p-value: 0.7401
```

```r
cor(perChangeMay, as.numeric(thatRaiseData2$PopDensity), use="complete.obs")
```

```
## [1] -0.02390509
```

May Housing Price:

```r
ggplot(thatRaiseData2, aes(x = as.numeric(`2020-06-30`), y = perChangeMay)) + 
  geom_point() + #alpha changes opaci
  geom_smooth(method = "lm")
```

<img src="figure/unnamed-chunk-25-1.png" title="plot of chunk unnamed-chunk-25" alt="plot of chunk unnamed-chunk-25" style="display: block; margin: auto;" />

```r
ggplot(lm(as.numeric(`2020-06-30`)~perChangeMay, data=thatRaiseData2)) + 
  geom_point(aes(x=.fitted, y=.resid))
```

<img src="figure/unnamed-chunk-25-2.png" title="plot of chunk unnamed-chunk-25" alt="plot of chunk unnamed-chunk-25" style="display: block; margin: auto;" />

```r
lm(perChangeMay ~ as.numeric(`2020-06-30`), data = thatRaiseData2)
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(`2020-06-30`), data = thatRaiseData2)
## 
## Coefficients:
##              (Intercept)  
##                1.466e-02  
## as.numeric(`2020-06-30`)  
##               -7.994e-09
```

```r
summary(lm(perChangeMay ~ as.numeric(`2020-06-30`), data = thatRaiseData2))
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(`2020-06-30`), data = thatRaiseData2)
## 
## Residuals:
##       Min        1Q    Median 
## -0.025204 -0.008255 -0.000425 
##        3Q       Max 
##  0.006480  0.079401 
## 
## Coefficients:
##                            Estimate
## (Intercept)               1.466e-02
## as.numeric(`2020-06-30`) -7.994e-09
##                          Std. Error
## (Intercept)               1.549e-03
## as.numeric(`2020-06-30`)  4.104e-09
##                          t value
## (Intercept)                9.465
## as.numeric(`2020-06-30`)  -1.948
##                          Pr(>|t|)
## (Intercept)                <2e-16
## as.numeric(`2020-06-30`)   0.0529
##                             
## (Intercept)              ***
## as.numeric(`2020-06-30`) .  
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01387 on 193 degrees of freedom
## Multiple R-squared:  0.01928,	Adjusted R-squared:  0.0142 
## F-statistic: 3.794 on 1 and 193 DF,  p-value: 0.05288
```

```r
cor(perChangeMay, as.numeric(thatRaiseData2$`2020-06-30`), use="complete.obs")
```

```
## [1] -0.1388512
```




```r
summary(lm(perChangeMay ~ as.numeric(`2020-06-30`) + as.numeric(PopDensity) + as.numeric(`PVI#`) + as.numeric(Covid.per.10.000) + as.numeric(MedianAge) + as.numeric(PercentPoverty) + CovidRestrictionsCategorical + Race + party + State + MedHouseIncomeCategorical + rurality, data = thatRaiseData2))
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(`2020-06-30`) + as.numeric(PopDensity) + 
##     as.numeric(`PVI#`) + as.numeric(Covid.per.10.000) + as.numeric(MedianAge) + 
##     as.numeric(PercentPoverty) + CovidRestrictionsCategorical + 
##     Race + party + State + MedHouseIncomeCategorical + rurality, 
##     data = thatRaiseData2)
## 
## Residuals:
##       Min        1Q    Median 
## -0.025853 -0.006268  0.000281 
##        3Q       Max 
##  0.004778  0.064859 
## 
## Coefficients: (2 not defined because of singularities)
##                                      Estimate
## (Intercept)                         3.003e-02
## as.numeric(`2020-06-30`)           -1.194e-08
## as.numeric(PopDensity)             -3.249e-07
## as.numeric(`PVI#`)                  1.210e-05
## as.numeric(Covid.per.10.000)        8.676e-06
## as.numeric(MedianAge)              -3.109e-04
## as.numeric(PercentPoverty)          2.324e-04
## CovidRestrictionsCategoricalLow    -2.517e-03
## CovidRestrictionsCategoricalMid    -9.460e-03
## RaceHispanic                       -8.075e-03
## RaceMixed                          -1.247e-03
## RaceWhite                          -2.951e-03
## partypurple                         3.907e-03
## partyRep                            5.886e-03
## StateFL                             2.012e-03
## StateIL                            -1.671e-02
## StateMD                            -1.083e-02
## StateNM                            -1.957e-03
## StatePA                            -6.466e-03
## StateSC                                    NA
## StateVA                                    NA
## MedHouseIncomeCategoricalLow       -8.581e-04
## MedHouseIncomeCategoricalMid       -2.611e-03
## MedHouseIncomeCategoricalVery High  8.238e-03
## MedHouseIncomeCategoricalVery Low  -4.993e-03
## ruralitySuburban                   -6.468e-03
## ruralityUrban                       6.993e-03
##                                    Std. Error
## (Intercept)                         1.338e-02
## as.numeric(`2020-06-30`)            6.469e-09
## as.numeric(PopDensity)              4.790e-07
## as.numeric(`PVI#`)                  1.825e-04
## as.numeric(Covid.per.10.000)        1.436e-05
## as.numeric(MedianAge)               1.758e-04
## as.numeric(PercentPoverty)          2.075e-04
## CovidRestrictionsCategoricalLow     6.803e-03
## CovidRestrictionsCategoricalMid     6.613e-03
## RaceHispanic                        5.654e-03
## RaceMixed                           4.814e-03
## RaceWhite                           4.268e-03
## partypurple                         3.670e-03
## partyRep                            4.569e-03
## StateFL                             3.949e-03
## StateIL                             6.417e-03
## StateMD                             6.564e-03
## StateNM                             7.129e-03
## StatePA                             6.475e-03
## StateSC                                    NA
## StateVA                                    NA
## MedHouseIncomeCategoricalLow        5.128e-03
## MedHouseIncomeCategoricalMid        3.713e-03
## MedHouseIncomeCategoricalVery High  6.671e-03
## MedHouseIncomeCategoricalVery Low   6.839e-03
## ruralitySuburban                    4.802e-03
## ruralityUrban                       4.120e-03
##                                    t value
## (Intercept)                          2.244
## as.numeric(`2020-06-30`)            -1.846
## as.numeric(PopDensity)              -0.678
## as.numeric(`PVI#`)                   0.066
## as.numeric(Covid.per.10.000)         0.604
## as.numeric(MedianAge)               -1.768
## as.numeric(PercentPoverty)           1.120
## CovidRestrictionsCategoricalLow     -0.370
## CovidRestrictionsCategoricalMid     -1.431
## RaceHispanic                        -1.428
## RaceMixed                           -0.259
## RaceWhite                           -0.691
## partypurple                          1.064
## partyRep                             1.288
## StateFL                              0.509
## StateIL                             -2.604
## StateMD                             -1.650
## StateNM                             -0.275
## StatePA                             -0.999
## StateSC                                 NA
## StateVA                                 NA
## MedHouseIncomeCategoricalLow        -0.167
## MedHouseIncomeCategoricalMid        -0.703
## MedHouseIncomeCategoricalVery High   1.235
## MedHouseIncomeCategoricalVery Low   -0.730
## ruralitySuburban                    -1.347
## ruralityUrban                        1.697
##                                    Pr(>|t|)
## (Intercept)                          0.0262
## as.numeric(`2020-06-30`)             0.0668
## as.numeric(PopDensity)               0.4986
## as.numeric(`PVI#`)                   0.9472
## as.numeric(Covid.per.10.000)         0.5465
## as.numeric(MedianAge)                0.0789
## as.numeric(PercentPoverty)           0.2645
## CovidRestrictionsCategoricalLow      0.7119
## CovidRestrictionsCategoricalMid      0.1545
## RaceHispanic                         0.1552
## RaceMixed                            0.7960
## RaceWhite                            0.4904
## partypurple                          0.2888
## partyRep                             0.1995
## StateFL                              0.6111
## StateIL                              0.0101
## StateMD                              0.1009
## StateNM                              0.7840
## StatePA                              0.3195
## StateSC                                  NA
## StateVA                                  NA
## MedHouseIncomeCategoricalLow         0.8673
## MedHouseIncomeCategoricalMid         0.4829
## MedHouseIncomeCategoricalVery High   0.2187
## MedHouseIncomeCategoricalVery Low    0.4664
## ruralitySuburban                     0.1799
## ruralityUrban                        0.0916
##                                     
## (Intercept)                        *
## as.numeric(`2020-06-30`)           .
## as.numeric(PopDensity)              
## as.numeric(`PVI#`)                  
## as.numeric(Covid.per.10.000)        
## as.numeric(MedianAge)              .
## as.numeric(PercentPoverty)          
## CovidRestrictionsCategoricalLow     
## CovidRestrictionsCategoricalMid     
## RaceHispanic                        
## RaceMixed                           
## RaceWhite                           
## partypurple                         
## partyRep                            
## StateFL                             
## StateIL                            *
## StateMD                             
## StateNM                             
## StatePA                             
## StateSC                             
## StateVA                             
## MedHouseIncomeCategoricalLow        
## MedHouseIncomeCategoricalMid        
## MedHouseIncomeCategoricalVery High  
## MedHouseIncomeCategoricalVery Low   
## ruralitySuburban                    
## ruralityUrban                      .
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01223 on 160 degrees of freedom
##   (10 observations deleted due to missingness)
## Multiple R-squared:  0.3346,	Adjusted R-squared:  0.2348 
## F-statistic: 3.352 on 24 and 160 DF,  p-value: 2.646e-06
```


```r
summary(lm(perChangeMay ~ as.numeric(`2020-06-30`) + as.numeric(PercentHispanic) + as.numeric(MedianAge) + as.numeric(PercentPoverty) + as.numeric(NumericalRestriction) + party + State + rurality, data = thatRaiseData2))
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(`2020-06-30`) + as.numeric(PercentHispanic) + 
##     as.numeric(MedianAge) + as.numeric(PercentPoverty) + as.numeric(NumericalRestriction) + 
##     party + State + rurality, data = thatRaiseData2)
## 
## Residuals:
##       Min        1Q    Median 
## -0.025116 -0.006510  0.000031 
##        3Q       Max 
##  0.005134  0.065561 
## 
## Coefficients: (1 not defined because of singularities)
##                                    Estimate
## (Intercept)                      -3.324e-02
## as.numeric(`2020-06-30`)         -6.707e-09
## as.numeric(PercentHispanic)      -1.384e-04
## as.numeric(MedianAge)            -3.876e-04
## as.numeric(PercentPoverty)        2.174e-04
## as.numeric(NumericalRestriction)  1.552e-03
## partypurple                       3.994e-03
## partyRep                          4.799e-03
## StateFL                          -1.311e-02
## StateIL                          -1.083e-02
## StateMD                           2.377e-03
## StateNM                           2.560e-02
## StatePA                          -3.515e-03
## StateSC                          -5.087e-02
## StateVA                                  NA
## ruralitySuburban                 -6.662e-03
## ruralityUrban                     6.178e-03
##                                  Std. Error
## (Intercept)                       3.184e-02
## as.numeric(`2020-06-30`)          4.305e-09
## as.numeric(PercentHispanic)       6.347e-05
## as.numeric(MedianAge)             1.638e-04
## as.numeric(PercentPoverty)        1.262e-04
## as.numeric(NumericalRestriction)  8.909e-04
## partypurple                       2.930e-03
## partyRep                          2.340e-03
## StateFL                           1.151e-02
## StateIL                           3.072e-03
## StateMD                           5.356e-03
## StateNM                           1.136e-02
## StatePA                           3.649e-03
## StateSC                           3.179e-02
## StateVA                                  NA
## ruralitySuburban                  4.375e-03
## ruralityUrban                     3.735e-03
##                                  t value
## (Intercept)                       -1.044
## as.numeric(`2020-06-30`)          -1.558
## as.numeric(PercentHispanic)       -2.181
## as.numeric(MedianAge)             -2.366
## as.numeric(PercentPoverty)         1.723
## as.numeric(NumericalRestriction)   1.743
## partypurple                        1.363
## partyRep                           2.051
## StateFL                           -1.138
## StateIL                           -3.527
## StateMD                            0.444
## StateNM                            2.254
## StatePA                           -0.963
## StateSC                           -1.600
## StateVA                               NA
## ruralitySuburban                  -1.523
## ruralityUrban                      1.654
##                                  Pr(>|t|)
## (Intercept)                      0.297852
## as.numeric(`2020-06-30`)         0.121060
## as.numeric(PercentHispanic)      0.030512
## as.numeric(MedianAge)            0.019062
## as.numeric(PercentPoverty)       0.086752
## as.numeric(NumericalRestriction) 0.083163
## partypurple                      0.174650
## partyRep                         0.041768
## StateFL                          0.256540
## StateIL                          0.000538
## StateMD                          0.657810
## StateNM                          0.025476
## StatePA                          0.336749
## StateSC                          0.111345
## StateVA                                NA
## ruralitySuburban                 0.129652
## ruralityUrban                    0.099872
##                                     
## (Intercept)                         
## as.numeric(`2020-06-30`)            
## as.numeric(PercentHispanic)      *  
## as.numeric(MedianAge)            *  
## as.numeric(PercentPoverty)       .  
## as.numeric(NumericalRestriction) .  
## partypurple                         
## partyRep                         *  
## StateFL                             
## StateIL                          ***
## StateMD                             
## StateNM                          *  
## StatePA                             
## StateSC                             
## StateVA                             
## ruralitySuburban                    
## ruralityUrban                    .  
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01197 on 174 degrees of freedom
##   (5 observations deleted due to missingness)
## Multiple R-squared:  0.3267,	Adjusted R-squared:  0.2687 
## F-statistic: 5.629 on 15 and 174 DF,  p-value: 2.625e-09
```



```r
summary(lm(perChangeMay ~ as.numeric(`2020-06-30`) + as.numeric(MedianAge) + as.numeric(PercentPoverty) + as.numeric(NumericalRestriction) + party + (State == "IL") + (rurality == "Urban"), data = thatRaiseData2))
```

```
## 
## Call:
## lm(formula = perChangeMay ~ as.numeric(`2020-06-30`) + as.numeric(MedianAge) + 
##     as.numeric(PercentPoverty) + as.numeric(NumericalRestriction) + 
##     party + (State == "IL") + (rurality == "Urban"), data = thatRaiseData2)
## 
## Residuals:
##       Min        1Q    Median 
## -0.026663 -0.006444 -0.000103 
##        3Q       Max 
##  0.005159  0.066036 
## 
## Coefficients:
##                                    Estimate
## (Intercept)                       3.574e-03
## as.numeric(`2020-06-30`)         -7.032e-09
## as.numeric(MedianAge)            -1.767e-04
## as.numeric(PercentPoverty)        2.849e-04
## as.numeric(NumericalRestriction)  1.578e-04
## partypurple                       4.000e-03
## partyRep                          4.427e-03
## State == "IL"TRUE                -1.001e-02
## rurality == "Urban"TRUE           9.634e-03
##                                  Std. Error
## (Intercept)                       7.645e-03
## as.numeric(`2020-06-30`)          4.251e-09
## as.numeric(MedianAge)             1.466e-04
## as.numeric(PercentPoverty)        1.174e-04
## as.numeric(NumericalRestriction)  8.140e-05
## partypurple                       2.682e-03
## partyRep                          2.147e-03
## State == "IL"TRUE                 2.345e-03
## rurality == "Urban"TRUE           2.537e-03
##                                  t value
## (Intercept)                        0.467
## as.numeric(`2020-06-30`)          -1.654
## as.numeric(MedianAge)             -1.205
## as.numeric(PercentPoverty)         2.426
## as.numeric(NumericalRestriction)   1.939
## partypurple                        1.491
## partyRep                           2.062
## State == "IL"TRUE                 -4.268
## rurality == "Urban"TRUE            3.797
##                                  Pr(>|t|)
## (Intercept)                        0.6407
## as.numeric(`2020-06-30`)           0.0998
## as.numeric(MedianAge)              0.2297
## as.numeric(PercentPoverty)         0.0163
## as.numeric(NumericalRestriction)   0.0541
## partypurple                        0.1376
## partyRep                           0.0406
## State == "IL"TRUE                3.17e-05
## rurality == "Urban"TRUE            0.0002
##                                     
## (Intercept)                         
## as.numeric(`2020-06-30`)         .  
## as.numeric(MedianAge)               
## as.numeric(PercentPoverty)       *  
## as.numeric(NumericalRestriction) .  
## partypurple                         
## partyRep                         *  
## State == "IL"TRUE                ***
## rurality == "Urban"TRUE          ***
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.01218 on 181 degrees of freedom
##   (5 observations deleted due to missingness)
## Multiple R-squared:  0.2743,	Adjusted R-squared:  0.2422 
## F-statistic: 8.552 on 8 and 181 DF,  p-value: 7.442e-10
```

```r
lm(perChangeMay ~ as.numeric(`2020-06-30`) + as.numeric(MedianAge) + as.numeric(PercentPoverty) + as.numeric(NumericalRestriction) + party + (State == "IL") + (rurality == "Urban"), data = thatRaiseData2) %>% 
  augment() #%>% 
```

```
## # A tibble: 190 x 16
##    .rownames perChangeMay
##    <chr>            <dbl>
##  1 1             0.0112  
##  2 2             0.00608 
##  3 3            -0.000605
##  4 4             0.00928 
##  5 5             0.0149  
##  6 6             0.00674 
##  7 7             0.0112  
##  8 8             0.0140  
##  9 9             0.0210  
## 10 10            0.00220 
## # … with 180 more rows, and 14
## #   more variables:
## #   as.numeric..2020.06.30.. <dbl>,
## #   as.numeric.MedianAge. <dbl>,
## #   as.numeric.PercentPoverty. <dbl>,
## #   as.numeric.NumericalRestriction. <dbl>,
## #   party <chr>,
## #   State.....IL. <lgl>,
## #   rurality.....Urban. <lgl>,
## #   .fitted <dbl>, .se.fit <dbl>,
## #   .resid <dbl>, .hat <dbl>,
## #   .sigma <dbl>, .cooksd <dbl>,
## #   .std.resid <dbl>
```

```r
  #ggplot(aes(x = .fitted, y = .resid, color = party)) + 
 # geom_point() +
 # geom_hline(yintercept = 0)
```
