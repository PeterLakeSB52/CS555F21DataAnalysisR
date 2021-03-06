#install.packages("pROC")
#install.packages("aod")
library(pROC)
library(aod)

setwd("C:/Users/Peter's Laptop/Documents/CS555F21DataAnalysisR/Assignments")

#Read CSV files 
data = read.csv("Assignment6Data.csv", header = TRUE)

#Create binary variable for the body temperature variable
data$temp_level = ifelse(data$temp >= 98.6, 1, 0)

#Summary by sex
temp.level.summary = table(data$sex, data$temp_level)
temp.level.summary
n.male = temp.level.summary[1,1] + temp.level.summary[1,2]
n.female = temp.level.summary[2,1] + temp.level.summary[2,2]
p.male = temp.level.summary[1,2] / n.male
p.female = temp.level.summary[2,2] / n.female

#Calculate Risk Difference
RD = p.male - p.female
p.hat = (temp.level.summary[1,2] + temp.level.summary[2,2]) / (n.male + n.female)
z = RD / sqrt((p.hat * (1 - p.hat)) * ((1/n.male) + (1/n.female)))
z
p = pnorm(z, mean = 0, sd = 1)
p

#Logistic Regression w/ sex as explanatory variable
#Add a dummy variable so that sex is [male = 0, female = 1]
data$Sex_dummy = ifelse(data$sex == 2, 1, 0)

model = glm(data$temp_level ~ data$Sex_dummy, family = binomial)
summary(model)

1.4469 / 0.3911

OR = exp(as.double(model$coefficients[2]))
OR

exp(cbind(OR = coef(model), confint.default(model)))

model.roc = roc(data$temp_level ~ data$Sex_dummy)
print(model.roc)

data$prob = predict(model, type=c("response"))
g.1 = roc(data$temp_level ~ data$prob)
g.1
plot(g.1)


#Logistic Regression w/ sex and heart rate as explanatory variables
model = glm(data$temp_level ~ data$Sex_dummy + data$Heart.rate, family = binomial)
summary(model)

#Global test of signiicance
wald.test(b=coef(model), Sigma = vcov(model), Terms = 2:3)

#Odds Ratios
OR.sex = exp(as.double(model$coefficients[2]))
OR.sex
OR.heart.rate = exp(as.double(model$coefficients[3]*10))
OR.heart.rate

model.roc = roc(data$temp_level ~ data$Sex_dummy + data$Heart.rate)
print(model.roc)

data$prob = predict(model, type=c("response"))
g.2 = roc(data$temp_level ~ data$prob)
g.2
plot(g.2)
