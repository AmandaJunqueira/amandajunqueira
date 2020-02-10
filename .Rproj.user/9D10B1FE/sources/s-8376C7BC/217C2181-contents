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
surveyData <- read.csv("Documents/Ubiqum/Data Exploratory Analysis/data/surveyBelkinElago.csv", sep = ";", dec = ",")

#Revaluing "No Response"
surveyData$brand <- revalue(surveyData$brand, c(" "="No Response"))


#exploration

#ggplot brand vs. salary
ggplot(surveyData, aes(salary, fill = brand)) + geom_histogram(bins = 100) +
  geom_vline(aes(xintercept = median(surveyData$salary))) + 
  labs(title = "Salary", subtitle = "and Brand") + scale_fill_manual(values = c("black", "royalblue", "red"))

ggplot(data=surveyData, aes(x=age, y=salary, colour=brand)) + geom_point() 



#plotting brand, salaty adn age
library(dplyr)
install.packages("dplyr")
surveyData <- sample_n(data_frame(), 5)


surveyData <- subset(surveyData,brand != " ")

p <- ggplot(data=surveyData, aes(x=age, y=salary, colour=brand, size=age)) + geom_point()
p + geom_point()

#Colorful Plot
q <- ggplot(data=surveyData, aes(x=age, y= salary, colour = brand)) 
q + geom_point()
q + geom_point(aes(colour=brand)) +
  scale_color_manual(values = c("brown", "bisque3")) +
  theme_bw() +
  labs(title = "Visual Map Towards Preferences", subtitle = "How Age And Salary influence brand preference") +
  guides(fill=guide_legend(title=NULL)) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(legend.title=element_blank()) +
  scale_x_continuous(name="Age") +
  scale_y_continuous(name="Salary")


 q + geom_line(size=5) + geom_point()


#age vs. brand
ggplot(surveyData, aes(age, fill = brand)) + 
  geom_histogram(bins=250) + 
  scale_fill_manual(values = c("brown", "bisque3")) +
  theme_bw() +
  labs(title = "Younger Customers Tend To Prefer Belkin") +
  guides(fill=guide_legend(title=NULL)) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(legend.title=element_blank()) +
  scale_x_continuous(name="Age") +
  scale_y_continuous(name="Count")

surveyData$elevel <- as.factor(surveyData$elevel)


#Brand Preference - In Numbers 
ggplot(surveyData, aes(brand)) + 
  geom_bar(aes(fill = brand)) +
  scale_fill_manual(values = c("brown", "bisque3")) + 
  theme_bw() +
  labs(title = "Brand Preference In Numbers", subtitle = "13% More Preference for Elago") +
  geom_text(x = 1, y = 4852, label = "4652") +
  geom_text(x = 2, y = 5548, label = "5348") + 
  guides(fill=guide_legend(title=NULL)) +
  scale_y_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  theme(plot.title = element_text(lineheight=.8, face="bold"))


surveyData %>% group_by(brand) %>% 
  summarise(n())
