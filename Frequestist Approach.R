# Data
library(MASS)
library(readxl)
data <- read_excel("RealEstate.xls")

## Random sample of 100
# index <- sample(c(1:350), size=100, replace =F)
# newData <- data[c(index),]
# save(newData, file = "newData.Rdata")

load("newData.Rdata")

# Naming Variables

## Sale Price
price <- newData$`Sale Price`
logPrice <- log(price)

## Age
age <- newData$Age
## Percent College - % in neighborhood who graduated college
college <- newData$`Pct College`
## # of Bedrooms
br <- newData$Bedrooms
## Lot size
size <- newData$`Lot Size`

fullMod <- lm(log(price) ~ age + college + br + size)
stepMod <- stepAIC(fullMod, direction = "both", trace = 1)

summary(stepMod)

# Removing college variable based on stepwise

finalMod <- lm(log(price) ~ age + br + size)
summary(finalMod)

plot(finalMod)

confint(finalMod)
