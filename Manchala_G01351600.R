library(tidyverse)

df<- read.csv("EmployeeAttrition.csv")
#q1
print(paste("The number of Columns in the Data set is ",ncol(df)))
print(paste("The number of Rows in the Data set is ",nrow(df)))

#q2
print(paste("The Maximum age in the Data set is ",max(df$Age)))

#q3
print(paste("The Minimum Daily rate in the Data set is ",min(df$DailyRate)))

#q4
print(paste("The Mean MontlyIncome in the Data set is ",mean(df$MonthlyIncome)))

#q5
print(paste("The number of employees rated WorkLifeBalance as  are :",nrow(df[df$WorkLifeBalance==1,])))

#q6

a<-nrow(df[df$TotalWorkingYears<=5,])
b<-nrow(df)
c<-a/b*100

print(paste("The percentage of employees who's TotalWorkingYears less than equal to 5 :",round(c)))
print(paste("The percentage of employees who's TotalWorkingYearsgreater than 5 :",round(100-c)))

#q7
d<- df %>%
  filter(Attrition =="Yes",RelationshipSatisfaction==1,YearsSinceLastPromotion>3)


#q8
male_set<- subset(df,Gender=="Male")
Female_set<- subset(df,Gender=="Female")



find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

print(paste("The Mean of environment satisfaction for Males is :",mean(male_set$EnvironmentSatisfaction)))
print(paste("The Median of environment satisfaction for Males is :",median(male_set$EnvironmentSatisfaction)))
print(paste("The Mode of environment satisfaction for Males is :",find_mode(male_set$EnvironmentSatisfaction)))
print(paste("The Standard Deviation of environment satisfaction for Males is :",sd(male_set$EnvironmentSatisfaction,na.rm=FALSE)))

mfd<- table(male_set$EnvironmentSatisfaction)
print(mfd)



print(paste("The Mean of environment satisfaction for Females is :",mean(Female_set$EnvironmentSatisfaction)))
print(paste("The Median of environment satisfaction for Females is :",median(Female_set$EnvironmentSatisfaction)))
print(paste("The Mode of environment satisfaction for Females is :",find_mode(Female_set$EnvironmentSatisfaction)))
print(paste("The Standard Deviation of environment satisfaction for Females is :",sd(Female_set$EnvironmentSatisfaction,na.rm=FALSE)))
ffd<- table(Female_set$EnvironmentSatisfaction)
print(ffd)



df1<-read.csv("Acme.csv")
#q1
str(df1)

#q2
summary(df1)

#q3

hist(df1$Years)
hist(df1$StSalary)

ggplot(df1,aes(x=df1$Gender))+
  geom_bar()
ggplot(df1,aes(x=df1$Degree))+
  geom_bar()

#q4
#a
plot(df1$Years,df1$StSalary)


#b
acmemale_set<- subset(df1,Gender=="M")
acmefemale_set<- subset(df1,Gender=="F")
plot(acmemale_set$Years,acmemale_set$StSalary)
plot(acmefemale_set$Years,acmefemale_set$StSalary)

#c
acmebs_set<- subset(df1,Degree=="BS")
acmems_set<- subset(df1,Degree=="MS")
acmephd_set<- subset(df1,Degree=="PhD")

plot(acmebs_set$Years,acmebs_set$StSalary)
plot(acmems_set$Years,acmems_set$StSalary)
plot(acmems_set$Years,acmems_set$StSalary)

#q5
#a
cor(acmemale_set$Years,acmemale_set$StSalary)
cor(acmefemale_set$Years,acmefemale_set$StSalary)

#b
cor(acmebs_set$Years,acmebs_set$StSalary)
cor(acmems_set$Years,acmems_set$StSalary)
cor(acmems_set$Years,acmems_set$StSalary)

#q6
#There is a gender bias while compares to the males and females according to their experience females salary was less although females are having more experience while compares to males
#Even though the co-relation between males years of experience their starting salary is less than females they are having high salaries