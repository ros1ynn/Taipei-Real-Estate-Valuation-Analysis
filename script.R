#import data
data <- read.csv(file = 'Real estate valuation data set.csv', head = TRUE)

#import libraries
library(funModeling)
library(Hmisc)

#data overview
df_status(data)
data_prof = profiling_num(data)
summary(data)

#create objects
age <- data$X2.house.age
distance <- data$X3.distance.to.the.nearest.MRT.station
stores <- data$X4.number.of.convenience.stores
price <- data$X7.house.price.of.unit.area

#correlation test and scatter plot
cor(price, age)
cor(price, distance)
cor(price, stores)

plot(age, price, main=" House Age vs. Price Per Unit", 
     xlab = 'age/year', ylab = 'price per unit/NTD')

plot(distance, price, main=" Distance to the Nearest MRT Stations  vs. Price Per Unit", 
     xlab = 'distance/step', ylab = 'price per unit/NTD')

plot(stores, price, main="Number of Convenience Stores vs. Price Per Unit", 
     xlab = 'stores', ylab = 'price per unit/NTD')


#simple linear regression
m <- lm(price ~ age, data = data)
m

#multiple linear regression
library(car)
m1 <- lm(price ~ stores + age, data = data)
m11 <- anova(m1, type=3)

m2 <- lm(price ~ distance + stores, data = data)
summary(m2)

m3 <- lm(price ~ age + distance, data = data)
summary(m3)

