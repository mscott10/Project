```{r, echo=FALSE,warning=FALSE}
library(knitr)
```

---
title: "Motor Trend Project"
output: word_document
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

```{r,results='hide'}
summary(mtcars)
```

```{r,results='hide'}
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

```{r}
reg<-lm(mpg~am,data=mtcars)
summary(reg)
```

amManual is positive (and thus better for mpg) when no other variables are included

```{r}
reg1<-lm(mpg~.,data=mtcars)
summary(reg1)
```

When all factors are included amManual is still positive. But a lot of these variables aren't significant, so we probably want to look at something else.

```{r,results='hide'}
best<-step(reg1,direction="both")
```

```{r}
summary(best)
```

Using a stepwise selection technique, amManual is still positive. And the adjusted R Square is much higher than it is in the model that includes only the transmission and the intercept. So we'll go with this as our best model. I looked at some models including interactions between amManual and other variables as well, but they produced results that were inconclusive for our questions. Basically, some interactions were positive, some were negative. So I was unable to determine whether manual or automatic transmissions were better in those cases.

In the Normal Q-Q plot, the shape of the tails worries me a little bit. It looks somewhat like the graph of x^3. But I have that same issue regardless of which linear model I choose. And it's possible this issue only exists because of the small sample size.

In this final model, manual transmission is better for mpg than an automatic transmission by about 1.8 mpg.



Appendix
---
```{r}
plot(best)
```
