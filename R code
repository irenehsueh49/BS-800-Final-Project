---
title: "BS 800 Final Project"
author: "Irene Hsueh"
date: "8/30/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(readxl)
library(tidyverse)
```

### Importing Excel File
```{r}
help <- read_excel("C:/Irene Hsueh's Documents/MS Applied Biostatistics/BS 800 - Accelerated Statistical Training/Module 15/HELP Data.xlsx") %>% 
#Selecting Variables for Analysis
  dplyr::select(id = idnum, 
                age = age, 
                sex = female, 
                drinks = "Drinks per Day") %>% 
#Sorting by Age
  arrange(age)
```



### Examining Data
```{r}
#Checking Missing Values
sum(is.na(help))

#Summary Statistics 
summary(help)
apply(help, 2, sd)

#Number of Subjects Who Reported No Drinking
sum(help$drinks==0)

#Histogram of Age
hist(help$age,
     xlab="Age",
     main="Histogram of Age",
     col="hotpink",
     breaks=25)

#Histogram of Drinks
hist(help$drinks,
     xlab="Average Drinks Per Day",
     main="Histogram of Number of Drinks",
     col="turquoise1",
     breaks=25)
```



### Linear Regression 
```{r}
#Linear Regression
linear_regression <- lm(drinks~age, data=help)
summary(linear_regression)

#Scatterplot of Drinks vs. Age
scatterplot <- plot(drinks~age, data=help, 
                    xlab="Age", 
                    ylab="Number of Drinks Per Day",
                    main="Scatterplot of Drinks vs. Age",
                    pch=20,
                    col="hotpink")

#Linear Regression Line
lines(x=help$age, y=linear_regression$fitted.values, col="cyan")

#Residual Plots 
residual_plot <- plot(linear_regression)

#Predicted Number of Drinks of a 50 Year Old
linear_regression$fitted.values[help$age==50]
linear_regression$coefficients[1] + linear_regression$coefficients[2]*50
```



### Log-Linear Regression
```{r}
#Logarithmic Transformation of Number of Drinks 
log_drinks <- log(help$drinks+1)

#Histogram of logdrinks
hist(log_drinks,
     xlab="Average Logdrinks Per Day",
     main="Histogram of Number of Logdrinks",
     col="seagreen1",
     breaks=25)

#Summary Statistics 
summary(log_drinks)
sd(log_drinks)

#Number of log_drinks for People Who Reported No Drinking
log_drinks[help$drinks==0]

#Log-Linear Regression
log_linear_regression <- lm(log_drinks~age, data=help)
summary(log_linear_regression)

#Scatterplot of logdrinks vs. Age
log_scatterplot <- plot(log_drinks~age, data=help, 
                        xlab="Age",
                        ylab="Logarithmic Number of Drinks Per Day",
                        main="Scatterplot of logdrinks vs. Age",
                        pch=20,
                        col="mediumpurple1")

#Log-Linear Regression Line
lines(x=help$age, y=log_linear_regression$fitted.values, col="hotpink")

#Residual Plots
residual_plot <- plot(log_linear_regression)

#Predicted Number of log_drinks of a 50 Year Old
log_linear_regression$fitted.values[help$age==50]
log_linear_regression$coefficients[1] + log_linear_regression$coefficients[2]*50

#Predicted Number of Drinks of a 50 Year Old
exp(log_linear_regression$coefficients[1] + log_linear_regression$coefficients[2]*50)-1
```



#Final Scatterplot
```{r}
#Scatterplot of Drinks vs. Age
scatterplot <- plot(drinks~age, data=help, 
                    xlab="Age", 
                    ylab="Number of Drinks Per Day",
                    main="Scatterplot of Drinks vs. Age",
                    pch=20,
                    col="hotpink")

#Linear Regression Line
lines(x=help$age, y=linear_regression$fitted.values, col="cyan2")

#Log-Linear Regression Line
lines(x=help$age, y=exp(log_linear_regression$fitted.values)-1, col="seagreen1")
```





