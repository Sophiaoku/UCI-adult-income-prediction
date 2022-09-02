library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)
library(Amelia)
library(caret)
library(rockchalk)

#1. Importing data
dt <- read.csv('adult_sal.csv')
head(dt)
summary(dt)
str(dt)


#2. Data Cleaning, Data wrangling and feature engineering 

#assign NA to missing values
dt[dt == "?"] <- NA

#know the amount of missing values present and if they can be dropped.
sum(is.na(dt))

#drop missing values because they consist of characters, so replacing values with a mean wont suffice
dt <- na.omit(dt)

#reduce the levels in some categories 
dt <- select(dt, -X)


dt$type_employer[dt$type_employer=="Without-pay"] <- "unemployed"
dt$education[dt$education == 'Preschool'] <- 'No education'

#modify atomic class

dt$type_employer <- as.factor(dt$type_employer)
dt$education <- as.factor(dt$education)
dt$marital <- as.factor(dt$marital)
dt$occupation <- as.factor(dt$occupation)
dt$race <- as.factor(dt$race)
dt$relationship <- as.factor(dt$relationship)
dt$sex <- as.factor(dt$sex)
dt$income <- as.factor(dt$income)

str(dt)

#combine redundant or similar groups to create less levels and easily data manipulation

#type_employer -> government, private, self employed and unemployed

table(dt$type_employer)

dt$type_employer <- combineLevels(dt$type_employer,levs = c("Self-emp-not-inc", "Self-emp-inc"), newLabel = c("Self-employed") )
dt$type_employer <- combineLevels(dt$type_employer,levs = c("Local-gov", "State-gov"), newLabel = c("State-Local") )

#education 

table(dt$education)

dt$education <- combineLevels(dt$education,levs = c('10th', '11th', '12th', '1st-4th','5th-6th','7th-8th','9th'), newLabel = c('Highschool-dropout'))

#marital status
table(dt$marital)
dt$marital <- combineLevels(dt$marital, levs = c('Divorced','Separated', 'Widowed'), newLabel = c('Not-Married'))
dt$marital <- combineLevels(dt$marital, levs = c('Married-AF-spouse','Married-civ-spouse', 'Married-spouse-absent'), newLabel = c('Married'))

#country grouped by continent 
table(dt$country)
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

dt$country <- sapply(dt$country,group_country)

dt$country <- as.factor(dt$country)
names(dt)[names(dt)=="country"] <- "region"
table(dt$region)

#final check on the data to see if there's anything left to clean or modify.
str(dt)
head(dt)
missmap(dt,col=c('yellow','black'),y.at=c(1),y.labels = c(''))

#3. Exploratory Data Analysis

par(mfrow=c(3,2))
par(mar = rep(2, 4))
hist(dt$age)
barplot(dt$sex)
barplot(dt$region)
hist(dt$hr_per_week)

ggplot(dt) + geom_bar(aes(x = marital, fill = income))
ggplot(dt) + geom_histogram(aes(x = age ,fill = income)) + theme_bw()
ggplot(dt,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_classic()
ggplot(dt) + geom_histogram(aes(x = hr_per_week ,fill = sex), bins=25, color = "black") + theme_bw()
ggplot(dt) + geom_histogram(aes(x = hr_per_week ,fill = income), bins=25,color = "black")

ggplot(dt) + geom_col()

#4. Building regression model 

head(dt)

#not necessary but for reproducibility 
set.seed(100) 
sample <- sample.split(dt$income, SplitRatio = 0.70)

# Training Data
train = subset(dt, sample == TRUE)

# Testing Data
test = subset(dt, sample == FALSE)

model = glm(income ~ ., family = binomial, data = train)
summary(model)

#calling step to remove non significant variables that don't add to the fit.
newstepmodel <- step(model)
summary(newstepmodel)


test$predicted.income = predict(model, newdata=test, type="response")

test$predicted <- as.factor(ifelse(test$predicted.income >0.5, '>50K','<=50K'))
tablemod <- table(test$income, test$predicted)

confusionMatrix(tablemod)
precision(tablemod)
recall(tablemod)
