###------------------
###Hypothesis Testing
###------------------

###Students Name: Charan Reddy
###GNumber: G 01351600

#Setting the Working Directory
getwd("~/Desktop")

rm(list=ls())

#Reading the dataset
Employee <- read.csv("EmployeeAttrition.csv", header = TRUE, sep = ",")
Employee

# Your hypothesis testings here...

#Questions

#Q1. If the Monthly Income of Males is greater than Females
#Q2. If the Work Life Balance of Males is less than Females 
#Q3. If the Years At Company of Single is less than Married
#Q4. If the Environmental Satisfaction of Attrition = Yes is less than Attrition = No 
#Q5. If the Monthly Income of Manager is greater than Laboratory Technician 
#Q6. If Years At Company and Daily Rate are correlated with each other
#Q7. If Years At Company and Monthly Income are correlated with each other
#Q8. If Years At Company varies depending on individual’s Marital Status
#Q9. If Monthly Income varies depending on individual’s Performance Rating 
#Q10. If Monthly Income varies depending on individual’s Work Life Balance

#Solutions

#A1
#Null: The Monthly Income of a male  is less than or equal to female
#Alternative: The Monthly Income of a male is greater to female

Male <- as.numeric(unlist(subset(Employee,Gender == "Male",select=MonthlyIncome)))
head(Male)
Male1 <- as.numeric(unlist(subset(Employee,Gender=="Female",select=MonthlyIncome)))
head(Male1)
wilcox.test(Male,Male1,paired=FALSE,alternative="greater")

#Reasoning:the null is accepted as the p-value - 0.9558 is greater than 0.05


#A2
#Null:The Work life balance of a male is greater than or equal to female
#Alternative:Worklifebalance a male is lesser to female

Employee2 <- as.numeric(unlist(subset(Employee,Gender=="Male",select=WorkLifeBalance)))
Employee3 <- as.numeric(unlist(subset(Employee,Gender=="Female",select=WorkLifeBalance)))
t.test(Employee2,Employee3,paired=FALSE,alternative="less")

#Reasoning:null is accepted as the p-value - 0.4577 is greater than 0.05

#A3
#Null:The Years of a company of a single is greater than or equal to married
#Alternative:The Years of a company of a single is lesser than married

Employee4 <- as.numeric(unlist(subset(Employee, MaritalStatus == "Single", select = YearsAtCompany)))
Employee5 <- as.numeric(unlist(subset(Employee, MaritalStatus == "Married", select = YearsAtCompany)))
wilcox.test(Employee4,Employee5, paired = FALSE, alternative = 'less')

#Reasoning:Null is rejected as the p-value - 0.001047 is less than 0.05
#Years at a Company of a Single is less than that of the married.

#A4
#Null:Environmental Satisfaction of an attrition Yes is greater than or equal to No
#Alternative:Environmental Satisfaction of an attrition Yes is less than No

Employee6<-as.numeric(unlist(subset(Employee,Attrition=="Yes",select=EnvironmentSatisfaction)))
Employee7<-as.numeric(unlist(subset(Employee,Attrition=="No",select=EnvironmentSatisfaction)))
t.test(Employee6,Employee7,paired=FALSE,alternative="less")

#Reasoning:null is rejected as the p-value 0.05 is greater than -0.0001046
#Environmental Satisfaction of an attrition yes less than to attrition no

#A5
#Null:The Monthly Income of a manager is less than or equal to the Laboratory Technician
#Alternative:The Monthly Income of Manager is greater than the Laboratory Technician

Employee8<-as.numeric(unlist(subset(Employee,JobRole=="Manager",select=MonthlyIncome)))
Employee9<-as.numeric(unlist(subset(Employee,JobRole=="Laboratory Technician",select=MonthlyIncome)))
wilcox.test(Employee8,Employee9,paired=FALSE,alternative="greater")

#Reasoning:null is rejected as the p-value - 2.2e-16 is less than 0.05

#A6
#Null:There is no specific correlation between the Years at a Company and DailyRate
#Alternative:There is a correlation between the Years at a Company and DailyRate

cor.test(Employee$YearsAtCompany,Employee$DailyRate, method = "pearson")

#Reasoning:The null is accepted as the p-value - 0.1919 is greater than 0.05

#A7
#Null: There is no specific correlation between the Years at a Company and Monthly Income
#Alternative: There is a correlation between Years at a Company and Monthly Income

cor.test(Employee$MonthlyIncome,Employee$YearsAtCompany,method="pearson")

#Reasoning: null is rejected as the p-value - 2.2e-16 is less than 0.05

#A8
#Null:The Years at a Company does not vary depending on the Marital Status
#Alternative:The Years at a Company does vary depending on the Marital Status

summary(aov(Employee$YearsAtCompany~Employee$MaritalStatus))

#Reasoning:null is accepted as the p-value 0.05 is greater than -0.0247

#A9
#Null: The MonthlyIncome does not vary depending on the PerformanceRating
#Alternative: The MonthlyIncome does vary depending on the PerformanceRating

summary(aov(Employee$MonthlyIncome~Employee$PerformanceRating))

#Reasoning: Null is accepted as the p-value - 0.512 is clearly greater than 0.05

#A10
#Null:The Monthly Income does not depend on the WorklifeBalance
#Alternative: The Monthly Income depends on the WorklifeBalance

summary(aov(Employee$MonthlyIncome~factor(Employee$WorkLifeBalance)))

#Reasoning:null is accepted as the p-value - 0.607 is greater than 0.05


