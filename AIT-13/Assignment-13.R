library(tidyverse)
install.packages("mclust")
library(mclust)
library(cluster)

#1
df<- read.csv("EmployeeAttrition.csv")
df

#a
ggplot(df,aes(x=TotalWorkingYears,y=MonthlyIncome))+
  geom_point()+
  geom_smooth()
#a) As the total working years increases monthly increases.


#b
ggplot(df,aes(x=Age,y=DistanceFromHome))+
  geom_point()+
  geom_smooth()
#b) No relation can be observed between Age and DistanceFromHome.

#c  
cor(df$TotalWorkingYears,df$MonthlyIncome)
cor(df$Age,df$DistanceFromHome)
#c) 
  # i) The correlation is 0.77 which supports our assumption.
  #ii) There is negative correlation between Age and DistanceFromHome.



#d
lmodel<- lm(TotalWorkingYears~MonthlyIncome,df)
print(lmodel)
summary(lmodel)
#d) The p value is less than Alpha value which means that this relationship is significant.


#2
#a

set.seed(1010103)
Centroids=3
df

df1<-df[ , c("HourlyRate", "TotalWorkingYears")] 
df1

kme<- kmeans(df1, centers=Centroids, nstart=1)
names(kme)
kme$cluster
df$cluster <- factor(kme$cluster)
df$cluster
centers <- data.frame(cluster=factor(1:Centroids), kme$centers)

centers

png("3groups.png", width = 5, height=4, units='in', res=400)
ggplot(data=df, aes(x=HourlyRate, y=TotalWorkingYears, shape=cluster, color=cluster)) +
  geom_point(alpha=.3) +scale_shape_manual(values = 1:Centroids,
                                           guide = guide_legend(override.aes=aes(size=1))) +
  geom_point(data=df,  aes(x=HourlyRate, y=TotalWorkingYears), size=2, stroke=2)  
dev.off()

#The 3 groups that we can see are 
#group-1: Employees that are having Hourly rate less than 50.
#group-2: Employees that are having Hourly rate less than 80 and more than 50.
#group-3: Employees that are having Hourly rate less than 100 and more than 80.





#clusters =5
Centroids=5


kme<- kmeans(df1, centers=Centroids, nstart=1)
names(kme)
kme$cluster
df$cluster <- factor(kme$cluster)
df$cluster
centers <- data.frame(cluster=factor(1:Centroids), kme$centers)

centers

png("5groups.png", width = 5, height=4, units='in', res=400)
ggplot(data=df, aes(x=HourlyRate, y=TotalWorkingYears, shape=cluster, color=cluster)) +
  geom_point(alpha=.3) +scale_shape_manual(values = 1:Centroids,
                                           guide = guide_legend(override.aes=aes(size=1))) +
  geom_point(data=df,  aes(x=HourlyRate, y=TotalWorkingYears), size=2, stroke=2)  
dev.off()

#The 5 groups that we can see are 
#group-1: Employees that are having Hourly rate less than 50.
#group-2: Employees that are having Hourly rate less than 70. and more than 50.
#group-3: Employees that are having Hourly rate less than 80hrs and has totalworkingYears less than 20.
#group-4: Employees that are having Hourly rate greater than 80 and has totalworkingYears less than 20.
#group-5: Employees that are having Hourly rate less than 80hrs and has totalworkingYears greater than 20.


