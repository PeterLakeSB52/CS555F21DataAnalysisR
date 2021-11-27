setwd("C:/Users/Peter's Laptop/Documents/CS555F21DataAnalysisR")

#Read CSV files 
data = read.csv("240_Feeder_data.csv", header = TRUE)

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
