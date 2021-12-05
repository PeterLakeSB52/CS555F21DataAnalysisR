setwd("C:/Users/Peter/Documents/CS555F21DataAnalysisR/Assignments")

#Read CSV files 
data = read.csv("Assignment6Data.csv", header = TRUE)

#Create binary variable for the body temperature variable
data$temp_level = ifelse(data$temp >= 98.6, 1, 0)

#Summary by sex
temp.level.summary = table(data$sex, data$temp_level)
n.male = temp.level.summary[1,1] + temp.level.summary[1,2]
n.female = temp.level.summary[2,1] + temp.level.summary[2,2]
p.male = temp.level.summary[1,2] / n.male
p.female = temp.level.summary[2,2] / n.female

#Calculate Risk Difference
RD = p.male - p.female
p.hat = (temp.level.summary[1,2] + temp.level.summary[2,2]) / (n.male + n.female)
z = RD / sqrt((p.hat * (1 - p.hat)) * ((1/n.male) + (1/n.female)))
