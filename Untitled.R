# ----------------------------------------------------------------- #
# Ubiqum - Module 2 - Sprint 1 
# Belkin vs Elago Survey: Exploratory DataAnalysis
# Author: Amanda Junqueira
# Date: February 2020
# Version 0.1
# ---------------------------------------------------------------- #

#loading libraries
library(ggplot2)
library(plyr)


#read data
surveyData <- read.csv("data/surveyBelkinElago.csv", sep = ";")

#Revaluing "No Response"
surveyData$brand <- revalue(surveyData$brand, c(" "="No Response"))

?remove


#exploration
surveyData$salary <- as.numeric(surveyData$salary)

#ggplot brand vs. salary
ggplot(surveyData, aes(salary, fill = brand)) + geom_histogram(bins = 100) +
geom_vline(aes(xintercept = median(surveyData$salary))) + 
labs(title = "Salary", subtitle = "and Brand") + scale_fill_manual(values = c("black", "royalblue", "red"))

ggplot(data=surveyData, aes(x=age, y=salary, colour=brand)) + geom_point()

p <- ggplot(data=surveyData, aes(x=age, y=salary, colour=age, size=age)) + geom_point()

p + geom_point()
q <- ggplot(data=surveyData, aes(x=age, y= salary, colour = brand))
q + geom_point()
q + geom_point(aes(colour=brand))


q + geom_line(size=2) + geom_point()

#age vs. brand
ggplot(surveyData, aes(age, fill = brand)) + 
  geom_histogram(bins=250) + 
  scale_fill_manual(values = c("black", "royalblue", "red "))

surveyData$elevel <- as.factor(surveyData$elevel)
ggplot(surveyData, aes(brand)) + 

  geom_bar(aes(fill = elevel)) +
  scale_fill_manual(values = c("black", "royalblue", "red", "dark green"))



ggplot(data=surveyData, aes(x=age, y=salary, colour=brand)) + geom_point()
surveyData <- surveyData[-c(2, 4, 6), ]

