

---
title: "Motor Trend Project"
output: pdf_document
---

Executive Summary
---

For this project, we were tasked with answering the following questions:

You work for Motor Trend, a magazine about the automobile industry. Looking at a data set of a collection of cars, they are interested in exploring the relationship between a set of variables and miles per gallon (MPG) (outcome). They are particularly interested in the following two questions: 

“Is an automatic or manual transmission better for MPG” 

"Quantify the MPG difference between automatic and manual transmissions"

To answer these questions, I did some exploratory analysis. First I ran a regression model including only an intercept and the transmission variable. In this case, a manual transmission led to a higher mpg. This was consistent with the results when more variables were included in the model.

The second question is a little more difficult to answer, since different models, of which more than one could be *correct*, produce different results. Based on the final model I chose, manual transmission is better for mpg than an automatic transmission by about 1.8 mpg.

Analysis
---


```r
summary(mtcars)
```


```r
data(mtcars)
mtcars$am[mtcars$am==0]<-"Automatic"
mtcars$am[mtcars$am==1]<-"Manual"
mtcars$am<-as.factor(mtcars$am)
mtcars$cyl<-as.factor(mtcars$cyl)
mtcars$vs<-as.factor(mtcars$vs)
mtcars$gear<-as.factor(mtcars$gear)
mtcars$carb<-as.factor(mtcars$carb)
str(mtcars)
```

All variables that should be categorical now are. Next I wanted to look at a model that includes only the variable for transmission, and an intercept.


```r
reg<-lm(mpg~am,data=mtcars)
summary(reg)
```

```
## 
## Call:
## lm(formula = mpg ~ am, data = mtcars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.3923 -3.0923 -0.2974  3.2439  9.5077 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   17.147      1.125  15.247 1.13e-15 ***
## amManual       7.245      1.764   4.106 0.000285 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.902 on 30 degrees of freedom
## Multiple R-squared:  0.3598,	Adjusted R-squared:  0.3385 
## F-statistic: 16.86 on 1 and 30 DF,  p-value: 0.000285
```

amManual is positive (and thus better for mpg) when no other variables are included


```r
reg1<-lm(mpg~.,data=mtcars)
summary(reg1)
```

```
## 
## Call:
## lm(formula = mpg ~ ., data = mtcars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.5087 -1.3584 -0.0948  0.7745  4.6251 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 23.87913   20.06582   1.190   0.2525  
## cyl6        -2.64870    3.04089  -0.871   0.3975  
## cyl8        -0.33616    7.15954  -0.047   0.9632  
## disp         0.03555    0.03190   1.114   0.2827  
## hp          -0.07051    0.03943  -1.788   0.0939 .
## drat         1.18283    2.48348   0.476   0.6407  
## wt          -4.52978    2.53875  -1.784   0.0946 .
## qsec         0.36784    0.93540   0.393   0.6997  
## vs1          1.93085    2.87126   0.672   0.5115  
## amManual     1.21212    3.21355   0.377   0.7113  
## gear4        1.11435    3.79952   0.293   0.7733  
## gear5        2.52840    3.73636   0.677   0.5089  
## carb2       -0.97935    2.31797  -0.423   0.6787  
## carb3        2.99964    4.29355   0.699   0.4955  
## carb4        1.09142    4.44962   0.245   0.8096  
## carb6        4.47757    6.38406   0.701   0.4938  
## carb8        7.25041    8.36057   0.867   0.3995  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.833 on 15 degrees of freedom
## Multiple R-squared:  0.8931,	Adjusted R-squared:  0.779 
## F-statistic:  7.83 on 16 and 15 DF,  p-value: 0.000124
```

When all factors are included amManual is still positive. But a lot of these variables aren't significant, so we probably want to look at something else.


```r
best<-step(reg1,direction="both")
```


```r
summary(best)
```

```
## 
## Call:
## lm(formula = mpg ~ cyl + hp + wt + am, data = mtcars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.9387 -1.2560 -0.4013  1.1253  5.0513 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 33.70832    2.60489  12.940 7.73e-13 ***
## cyl6        -3.03134    1.40728  -2.154  0.04068 *  
## cyl8        -2.16368    2.28425  -0.947  0.35225    
## hp          -0.03211    0.01369  -2.345  0.02693 *  
## wt          -2.49683    0.88559  -2.819  0.00908 ** 
## amManual     1.80921    1.39630   1.296  0.20646    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.41 on 26 degrees of freedom
## Multiple R-squared:  0.8659,	Adjusted R-squared:  0.8401 
## F-statistic: 33.57 on 5 and 26 DF,  p-value: 1.506e-10
```

Using a stepwise selection technique, amManual is still positive. And the adjusted R Square is much higher than it is in the model that includes only the transmission and the intercept. So we'll go with this as our best model. I looked at some models including interactions between amManual and other variables as well, but they produced results that were inconclusive for our questions. Basically, some interactions were positive, some were negative. So I was unable to determine whether manual or automatic transmissions were better in those cases.

In the Normal Q-Q plot, the shape of the tails worries me a little bit. It looks somewhat like the graph of x^3. But I have that same issue regardless of which linear model I choose. And it's possible this issue only exists because of the small sample size.

In this final model, manual transmission is better for mpg than an automatic transmission by about 1.8 mpg.



Appendix
---

```r
plot(best)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) ![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-2.png) ![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-3.png) ![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-4.png) 

