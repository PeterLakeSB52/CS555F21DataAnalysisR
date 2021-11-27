#setwd("C:/Users/Peter's Laptop/Documents/CS555F21DataAnalysisR")
install.packages("geosphere")
library(geosphere)

#Read CSV files 
data = read.csv("240_Feeder_data.csv", header = TRUE)

#Calculate distance from Unis to BU
lon = as.vector(data$Longitude)
lat = as.vector(data$Latitude)
BULon = as.vector(rep(-71.10640804519835, times=239))
BULat = as.vector(rep(42.350252194515846, times=239))
distance = distHaversine(cbind(lon, lat), cbind(BULon, BULat))
data$distance = round((distance*0.000621371), 2)

#Distribution of response variable
hist(data$BU_2020_Apps)
boxplot(data$BU_2020_Apps)

#Distribution of response variable after ln transformation
hist(log(data$BU_2020_Apps))
data$BU_Log_Apps = log(data$BU_2020_Apps)

#Visualizing relationship between various predictors and the response variable
plot(data$BU_2020_Apps, data$X2020.Applied.Count)

#Multiple Linear Regression
model = lm(data$BU_2020_Apps ~ data$UGDS + data$X2020.Applied.Count + data$LSATMedDiff + data$GPAMedDiff)
summary(model)
plot(data$BU_2020_Apps, model$residuals)
hist(log(data$BU_2020_Apps))
