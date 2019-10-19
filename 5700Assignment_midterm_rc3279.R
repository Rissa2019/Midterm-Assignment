#Midterm Assignment Rissa Cao
#install.packages('aod')
#install.packages("rattle")

library(data.table)
library(ggplot2)
library(aod)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(grid)
library(dplyr)
library(gridExtra)

#Load Data
attrition <- as.data.table(read.csv("assignment 2.csv", encoding = "UTF-8"))

# Select R&D with no attrition
noAttritionRd <- attrition[Attrition == "No" & Department == "Research & Development"]

# Analysis for Question 2 ------------------------------------------------------------------------------------------------------------------
nrow(noAttritionRd) # 828 employees
sum(noAttritionRd$MonthlyRate) # 11846425 monthly rate
sum(noAttritionRd$MonthlyIncome) # 5489910 monthly income
(sum(noAttritionRd$MonthlyRate))*12 # 142157100 yearly rate
(sum(noAttritionRd$MonthlyIncome))*12 # 65878920 yearly income
((sum(noAttritionRd$MonthlyIncome))*12)/nrow(noAttritionRd) # average yearly income per employee 79563.91
nrow(noAttritionRd[Gender == "Male"]) # 492 males
nrow(noAttritionRd[Gender == "Female"]) #336 females
((nrow(noAttritionRd[Gender == "Male"]))/(nrow(noAttritionRd)))*100 # 59.4Z% male

# Additional key metrics
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(noAttritionRd$Education) # 3 (bachelors)
getmode(noAttritionRd$MaritalStatus) # the majority are married
((nrow(noAttritionRd[MaritalStatus == "Married"]))/(nrow(noAttritionRd)))*100 #46.38% are married
mean(noAttritionRd$TotalWorkingYears) # 11.87 average total working years
mean(noAttritionRd$YearsAtCompany) # 7.17 average years at company
getmode(noAttritionRd$EducationField) # life sciences is the most common education field
getmode(noAttritionRd$JobRole) # research scientist is the most common job role
getmode(noAttritionRd$PerformanceRating) # 3 (excellent)

# Analysis Question 3 ----------------------------------------------------------------------------------------------------------------------
# Select only R&D Division (this time including attrition)
RD <- attrition[Department == "Research & Development"]
nrow(RD) # There are 961 

# Convert to factor categorical variabls
str(RD)
cols <- c("Education", "EnvironmentSatisfaction", "JobInvolvement", "PerformanceRating", "JobLevel", "JobSatisfaction", "RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance")
RD[,(cols):=lapply(.SD, as.factor),.SDcols=cols]

# Regression to determine relationship between salary hike and performance rating
model1 <- lm(PercentSalaryHike ~ PerformanceRating, data = RD)
summary(model1) # There is a very strong correlation. Higher percent salary hike correlates strongly with a performance rating of 4 ("Outstanding")

# Let us treat the performance rating as a continuous variable to see if the longer the person has been in their current role or with the company, the higher the performance rating
# Linear regression with performance rating as a function of years in current role
model2 <- lm(as.numeric(RD$PerformanceRating) ~ RD$YearsInCurrentRole, data=RD)
summary(model2) # There is not a strong relationship
cor(x=RD$YearsInCurrentRole, y=as.numeric(RD$PerformanceRating), method = "pearson") # -0.01021174 correlation of almost 0
# Linear regression with performance rating as a function of years at company
model3 <- lm(as.numeric(RD$PerformanceRating) ~ RD$YearsAtCompany, data=RD)
summary(model3) # There is a stronger relationship but not significant
cor(x=RD$YearsAtCompany, y=as.numeric(RD$PerformanceRating), method = "pearson") # -0.04119516 correlation of almost 0

# Now let us see if the longer a person has been in their current role or with the company leads to higher percent salary hile (as a measure of good performance)
# Linear regression with salary hike as a function of years at current role
model4 <- lm(RD$PercentSalaryHike ~ RD$YearsInCurrentRole, data=RD)
summary(model4) # There is not a strong relationship
cor(x=RD$YearsInCurrentRole, y=RD$PercentSalaryHike, method = "pearson") # -0.05051549 correlation of almost 0
scatter.smooth(x=RD$YearsInCurrentRole, y=RD$PercentSalaryHike, main="Salary Hike ~ Years in Current ROle") # there is a slight negative trend 
# Linear regression with salary hike as a function of years at company
model5 <- lm(RD$PercentSalaryHike ~ RD$YearsAtCompany, data=RD)
summary(model5) # There is not a strong relationship
cor(x=RD$YearsAtCompany, y=RD$PercentSalaryHike, method = "pearson") # -0.07330911 correlation of almost 0
scatter.smooth(x=RD$YearsAtCompany, y=RD$PercentSalaryHike, main="Salary Hike ~ Years at Company") # there is a slight negative trend
# Experience and performance as determinants for atrition
model6 <- glm(Attrition ~ PerformanceRating + PercentSalaryHike + YearsAtCompany + YearsInCurrentRole, data = RD, family = "binomial")
summary(model6) # Number of years in current role has the strongest impact on attrition
coef(model6) # But the trend is negative

# Analysis Question 4 ----------------------------------------------------------------------------------------------------------------------
# Run another logistic regression with all relevant variables to identify high risk employees
model7 <- glm(Attrition ~ Age + BusinessTravel + DistanceFromHome + Education + EducationField + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, data = RD, family = "binomial")
summary(model7)
# Decision tree
decisionTreeModel= rpart(Attrition ~ Age + BusinessTravel + DistanceFromHome + Education + EducationField + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager,data=RD,method="class",minbucket = 25)
fancyRpartPlot(decisionTreeModel) # Job level of 2 through 5 less likely to leave and those who work overtime are more likely to leave
# Heat Maps Continuous Attributes
p1 <- RD %>% ggplot(aes(x = MonthlyIncome, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Monthly Income") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

p2 <- RD %>% ggplot(aes(x = PercentSalaryHike, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Percentage Salary Hike") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

p3 <- RD %>%  ggplot(aes(x = YearsAtCompany, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years At Company") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

p4 <- RD %>% ggplot(aes(x = YearsInCurrentRole, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years in Current Role") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

p5 <- RD %>% ggplot(aes(x = YearsSinceLastPromotion, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years Since Last Promotion") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

p6 <- RD %>% ggplot(aes(x = YearsWithCurrManager, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Years With Current Manager") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

p7 <- RD %>% ggplot(aes(x = Age, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Age") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

p8 <- RD %>% ggplot(aes(x = DistanceFromHome, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Distance From Home")  + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

p9 <- RD %>% ggplot(aes(x = NumCompaniesWorked, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Number of Companies")  + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

p10 <- RD %>% ggplot(aes(x = TotalWorkingYears, fill = Attrition)) + geom_density(alpha = 0.5) + ggtitle("Total Working Years")  + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank())

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, nrow = 5, ncol = 2)

# Bar Charts Categorical Attributes
p11 <- RD %>%
  group_by(Gender) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = Gender, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Gender") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 20))


p12 <- RD %>%
  group_by(Education) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = Education, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Education") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 20))

p13 <- RD %>%
  group_by(EducationField) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = EducationField, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Education Field") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))

p14 <- RD %>%
  group_by(MaritalStatus) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = MaritalStatus, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Marital Status") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))

p15 <- RD %>%
  group_by(RelationshipSatisfaction) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = as.factor(RelationshipSatisfaction), y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Relationship Satisfaction") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))

p16 <- RD %>%
  group_by(WorkLifeBalance) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = as.factor(WorkLifeBalance), y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Work Life Balance") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 35))

p17 <- RD %>%
  group_by(BusinessTravel) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = BusinessTravel, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Business Travel") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))

p18 <- RD %>%
  group_by(EnvironmentSatisfaction) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = EnvironmentSatisfaction, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Environment Satisfaction") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))

p19 <- RD %>%
  group_by(JobInvolvement) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = JobInvolvement, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Job Involvement") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 40))

p20 <- RD %>%
  group_by(JobSatisfaction) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = JobSatisfaction, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Job Satisfaction") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 30))

p21 <- RD %>%
  group_by(OverTime) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = OverTime, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Over Time") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 35))

p22 <- RD %>%
  group_by(PerformanceRating) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = as.factor(PerformanceRating), y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Performance Rating") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)+ scale_y_continuous(limits = c(0, 20))

p23 <- RD %>%
  group_by(JobRole) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = JobRole, y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Job Role") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

p24 <- RD %>%
  group_by(JobLevel) %>%
  summarise(attrition_rate = round((sum(if_else(Attrition == "Yes",1,0))/n()*100),2)) %>%
  ggplot(aes(x = as.factor(JobLevel), y = attrition_rate))+ geom_bar(stat = 'identity',fill = "gray50") + ggtitle("Attrition Rate - Job Level") + theme(plot.title = element_text(size =10),axis.text.x = element_text(size =7,angle = 45, hjust = 1),axis.title.x=element_blank()) +geom_text(aes(label=attrition_rate), size = 2.5, position=position_dodge(width=0.2), vjust=-0.25)

grid.arrange(p11, p12, p13, p14, ncol = 2)
grid.arrange(p15, p16, p17, p18, ncol = 2)
grid.arrange(p19, p20, p21, p22, ncol = 2)
grid.arrange(p23, p24, ncol = 2)


