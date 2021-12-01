#setwd("C:/Users/Peter's Laptop/Documents/CS555F21DataAnalysisR")
#install.packages("geosphere")
#install.packages('ggplot2')
library(ggplot2)
library(geosphere)


#Read CSV files 
data = read.csv("240_Feeder_data.csv", header = TRUE)

#Calculate distance from Unis to BU
lon = as.vector(data$Longitude)
lat = as.vector(data$Latitude)
BULon = as.vector(rep(-71.10640804519835, times=239))
BULat = as.vector(rep(42.350252194515846, times=239))
distance = distHaversine(cbind(lon, lat), cbind(BULon, BULat))
#Converting meters to miles and saving to the dataframe
data$distance = round((distance*0.000621371), 2)

#Distribution of response variable
Apps.hist = ggplot(data, aes(BU_2020_Apps)) + geom_histogram(binwidth = 1, fill = "dodgerblue")
Apps.hist = Apps.hist + ggtitle("Histogram of BU Applications per Institution") +
  xlab("# of Applications per Institution") + ylab("Frequency") + theme(plot.title = element_text(hjust = .5))
Apps.hist
Apps.box = ggplot(data, aes(BU_2020_Apps)) + geom_boxplot()
Apps.box = Apps.box + xlab("# of Applications per Institution")
Apps.box

#Distribution of response variable after ln transformation
data$BU_Log_Apps = log(data$BU_2020_Apps)
#Removing negative inf values
data = data[data$BU_Log_Apps>=0,]
Log.Apps.hist = ggplot(data, aes(BU_Log_Apps)) + geom_histogram(binwidth = 1, fill = "dodgerblue")
Log.Apps.hist = Log.Apps.hist + ggtitle("Histogram of the Natural Log of BU Applications per Institution") +
  xlab("ln(# of Applications per Institution)") + ylab("Frequency") + theme(plot.title = element_text(hjust = .5))
Log.Apps.hist


#Scatterplot matrix
data = data[,c(4,7,8,9,16,10,11,12,13,14,15)]
pairs(data[,5:11], pch = 19, col = "dodgerblue")

#Correlation coefficients
cor(data$LSATMedDiff, data$BU_Log_Apps)
cor(data$GPAMedDiff, data$BU_Log_Apps)
cor(data$UGDS, data$BU_Log_Apps)
cor(data$X2020.Applied.Count, data$BU_Log_Apps)
cor(data$X2020.Matriculant.Count, data$BU_Log_Apps)
cor(data$distance, data$BU_Log_Apps)

#Backward Elimination Multiple Linear Regression
#Iteration 1
model = lm(data$BU_Log_Apps ~ data$UGDS + data$X2020.Applied.Count + data$X2020.Matriculant.Count 
           + data$LSATMedDiff + data$GPAMedDiff)
summary(model)
#Iteration 2
model = lm(data$BU_Log_Apps ~ data$UGDS + data$X2020.Applied.Count + data$X2020.Matriculant.Count 
           + data$LSATMedDiff)
summary(model)
#Iteration 3
model = lm(data$BU_Log_Apps ~ data$UGDS + data$X2020.Applied.Count + data$LSATMedDiff)
summary(model)
#Iteration 4
model = lm(data$BU_Log_Apps ~ data$X2020.Applied.Count + data$LSATMedDiff)
summary(model)
anova(model)

#Plot of regression
model.plot = ggplot(data, aes(y = BU_Log_Apps, x = X2020.Applied.Count, color = LSATMedDiff))+geom_point()+stat_smooth(method="lm",se=FALSE)
model.plot = model.plot + ggtitle("Plot of Multiple Linear Regression Model") +
  xlab("# of Applicants (Nationally) for each Institution") + ylab("ln(# of Applications to BU)") + theme(plot.title = element_text(hjust = .5))
model.plot

#Residual Plots
#Plot of model residuals
Resid.hist = ggplot(model, aes(resid(model))) + geom_histogram(fill = "dodgerblue")
Resid.hist = Resid.hist + ggtitle("Histogram of Model Residuals") +
  xlab("Residual Values") + ylab("Frequency") + theme(plot.title = element_text(hjust = .5))
Resid.hist

#Fitted x resdiuals
fittedresid.plot = ggplot(model, aes(y = resid(model), x = fitted(model)))+geom_point(colour = "dodgerblue")+geom_abline(intercept = 0, slope = 0, colour = "red")
fittedresid.plot

#2020Apps resdiuals
Appsresid.plot = ggplot(data, aes(y = resid(model), x = X2020.Applied.Count))+geom_point(colour = "dodgerblue")+geom_abline(intercept = 0, slope = 0, colour = "red")
Appsresid.plot

#LSATMedDiff resdiuals
LSATMedDiffresid.plot = ggplot(data, aes(y = resid(model), x = LSATMedDiff))+geom_point(colour = "dodgerblue")+geom_abline(intercept = 0, slope = 0, colour = "red")
LSATMedDiffresid.plot

#Global F test
#Null hypothesis: There is no linear association at a = 0.05
n = nrow(data)
f_test = qf(.95, df1 = 2, df2 = n - 2 - 1)
global_F = summary(model)$fstatistic[1]
global_F >= f_test
#Reject null hypothesis, there is a linear association at a = 0.05

#Pairwise t test
#Null hypothesis Beta 2020.Applied.Count = 0 
n = nrow(data)
df = n - 2 -1
t_test = qt(0.975, df = df)
model.summary = summary(model)
Beta.Applied = model.summary$coefficients[2,1]
SE.Applied = model.summary$coefficients[2,2]
t.applied = Beta.Applied/SE.Applied
t.applied >= t_test
#Reject null hypothesis, 2020 Applied count is a significant predictor at a = 0.05

#Pairwise t test
#Null hypothesis Beta LSATMedDiff = 0 
Beta.LSAT = model.summary$coefficients[3,1]
SE.LSAT = model.summary$coefficients[3,2]
t.LSAT = Beta.LSAT/SE.LSAT
t.LSAT >= t_test
#Reject null hypothesis, LSATMedDiff is a significant predictor at a = 0.05