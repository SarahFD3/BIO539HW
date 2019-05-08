
#Contain plots that illustrate relationship between variables and case where relationship do not exist
#Contain plots with data divided by category as needed
#Include captions and descriptions of plots
#Show only informative output (not code or raw results)
#Formate text and results appropriately including headers and inline code. 
#Include R-squared and p-values from relevant statisical tests as necessary to support relationships
install.packages("stringr")
install.packages("reshape2")

library(tidyverse)
library(ggplot2)
library(reshape2)
library(dbplyr)
library(stringr)

fed_spend <- read.csv("fed_r_d_spending.csv")
energy_spend <- read.csv("energy_spending.csv")
climate_spend <- read.csv("climate_spending.csv")

summary(fed_spend)
summary(energy_spend)
summary(climate_spend)

View(fed_spend)
View(energy_spend)
View(climate_spend)

fed_spend$department
energy_spend$department
climate_spend$department

#Federal
#plot each budget over year by department
attach(fed_spend)

ggplot(fed_spend, aes(x=year, y=rd_budget, color=department))+
  geom_line()+
  scale_color_discrete(name="Department")+
  labs(title="R&D Budgets by Department")+
  xlab("Year")+
  ylab("R&D Budget ($)")

ggplot(fed_spend, aes(year, discretionary_outlays, color=department))+
  geom_boxplot()

ggplot(fed_spend, aes(year, total_outlays, color=department))+
  geom_boxplot()

DOD_spend <- subset(fed_spend, department=="DOD")

ggplot(DOD_spend, aes(year))+
  geom_line(aes(y=rd_budget), color="red")+
  geom_line(aes(y=total_outlays), color="blue")+
  geom_line(aes(y=discretionary_outlays), color="green")+
  scale_color_discrete(name="Budgets")+
  labs(title="DOD")+
  xlab("Year")+
  ylab("Total Spending ($)")

ggplot(DOD_spend, aes(year))+
  geom_line(aes(y=rd_budget), color = "green")+
  geom_line(aes(y=total_outlays), color="blue")+
  geom_line(aes(y=discretionary_outlays), color="red")+
  scale_color_discrete(name="Budgets")+
  labs(title="DOD Over Time")+
  xlab("Year")+
  ylab("Total Spending ($)")

plot(total_outlays,gdp)
fit=lm(total_outlays~gdp)
summary(fit)
abline(fit)


#Energy

attach(energy_spend)
class(energy_spend$department)
class(energy_spend$year)
class(energy_spend$energy_spending)

ggplot(energy_spend, aes(year, energy_spending, color=department))+
  geom_line()+ scale_color_discrete(name="Department")+
  ylab("Total Spent") + xlab("Year")


#Climate
View(climate_spend)
attach(climate_spend)
NASA_climate <- subset(climate_spend, climate_spend$department=="NASA") 

View(NASA_climate)
MergedNASA <- merge(NASA_spend, NASA_climate)
head(MergedNASA)
View(MergedNASA)
