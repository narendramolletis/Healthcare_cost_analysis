# installing and loading packages to read a excel file
install.packages("readxl")
library(readxl)

# installing and loading dplyr for data manipulation 
install.packages("dplyr")
library(dplyr)

# setting working directory
setwd("C:/Users/molle/Downloads/Simplilearn/Self learning/Data science with R/Project/Project/Projects for Submission/Healthcare/Healthcare")

# know current working directory
getwd
# reading the csv file to a data frame()

hospital_cost <-  read.csv("HospitalCosts.csv")
tail(hospital_cost)

# total number of columns and rows
dim(hospital_cost)

# Structure of data set
str(hospital_cost)

# Summary 
summary(hospital_cost)

table(hospital_cost $AGE)
table(hospital_cost $FEMALE)
table(hospital_cost$LOS)
table(hospital_cost $RACE)
table(hospital_cost$TOTCHG)
table(hospital_cost$APRDRG) 

### Question 1. To record the patient statistics, 
# the  agency wants to find the age category of people who frequents the hospital and has the maximum expenditure. 
# 1.
hist(hospital_cost$AGE, main = "Frequency of  patients in each AGE group",col = "green",xlab = "Age")
table(hospital_cost $AGE)
aggregate(TOTCHG~AGE,FUN=sum,data = hospital_cost)

# Question 2. In order of severity of the diagnosis and treatments and to find out the expensive treatments,
#the agency wants to find the diagnosis related group that has maximum hospitalization and expenditure.
hist(hospital_cost$APRDRG, col = "green" ,main = "frequency of diagnosis groups ",xlab = "Groups")
APRDRG_fact<-as.factor(hospital_cost$APRDRG)
summary(APRDRG_fact)
which.max(summary(APRDRG_fact)) 

diagnosiscost <- aggregate(TOTCHG ~ APRDRG, FUN = sum, data = hospital_cost)
diagnosiscost
diagnosiscost[which.max(diagnosiscost$TOTCHG),]

# 3
hospital_cost<-na.omit(hospital_cost) # removing null values
summary(hospital_cost) # no null values

hospital_cost$RACE<-as.factor(hospital_cost$RACE) 

model_aov<-aov(TOTCHG~RACE,data = hospital_cost) 

model_aov#ANOVA RESULTS

summary(model_aov) 

summary(hospital_cost$RACE) #getting max hospital cost per race 

# 4
hospital_cost$FEMALE<-as.factor(hospital_cost$FEMALE) 

lr<-lm(TOTCHG~AGE+FEMALE,data = hospital_cost) #calling Regression function 

summary(lr)

summary(hospital_cost$FEMALE) 

# 5
hospital_cost$RACE<-as.factor(hospital_cost$RACE) 

lr2<-lm(LOS~AGE+FEMALE+RACE,data = hospital_cost) 

summary(lr2)


#6
lr3<-lm(TOTCHG~AGE+FEMALE+RACE+LOS+APRDRG,data = hospital_cost) 

summary(lr3) 
