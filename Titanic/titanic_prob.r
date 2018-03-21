setwd("C:/Users/ravin/Desktop/SEM 3")
dummy_train=read.csv("C:\\Users\\ravin\\Desktop\\SEM 3\\train.csv", header=T)
table(titanic_train$Survived)
prop.table(table(titanic_train$Survived))
nrow(test)
test$Survived<-rep(0, 418)
submit<- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyalldie.csv", row.names = FALSE)
table(titanic_train$Sex,titanic_train$Survived)
prop.table(table(titanic_train$Sex,titanic_train$Survived),1)
test$Survived <- 0
test$Survived[test$Survived == 'Female'] <- 1
summary(titanic_train$Age)
sum(is.na(titanic_train$Survived))
sum(is.na(titanic_train$Pclass))
sum(is.na(titanic_train$Name))
sum(is.na(titanic_train$Sex))
sum(is.na(titanic_train$Age))
sum(is.na(titanic_train$SibSp))
sum(is.na(titanic_train$Parch))
sum(is.na(titanic_train$Ticket))
sum(is.na(titanic_train$Fare))
sum(is.na(titanic_train$Cabin))
sum(is.na(titanic_train$Embarked))
titanic_train$Age[is.na(titanic_train$Age)] <- 28
sum(is.na(titanic_train$Age))
titanic_train$Child <- 0
titanic_train$Child[titanic_train$Age < 18] <- 1
table(titanic_train$Child,titanic_train$Sex)
aggregate(Survived~Child + Sex, data=titanic_train, FUN=sum)
#head(titanic_train$Ticket)
aggregate(Survived~Child + Sex, data=titanic_train, FUN = length)# target variable is on the left side of tilda
aggregate(Survived~Child + Sex, data=titanic_train, FUN = mean)

#titanic_train$Fare<-dummy_train$Fare
#creating bins for fare 
titanic_train$Fare2 <- '30+' # bin one 30 or more
titanic_train$Fare2[titanic_train$Fare < 30 & titanic_train$Fare >= 20]<-'20-30'# bin two for 20-30
titanic_train$Fare2[titanic_train$Fare <20 & titanic_train$Fare >= 10]<-'10-20' # bin three for 10-20
titanic_train$Fare2[titanic_train$Fare<10] <- '<10' # bin for 10 or less

aggregate(Survived~Fare2 + Pclass + Sex, data=titanic_train, FUN=function(x){sum(x)/length(x)})
summary(titanic_train$Fare2)


install.packages('rattle')
installed.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
fit<-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = titanic_train, method = 'class' )
plot(fit)
text(fit)
fancyRpartPlot(fit)

test$Survived<-0
test$Survived[test$Sex=='female']<-1
test$Survived[test$Sex=='female' & test$Pclass==3 & test$Fare>=20]<-0
summary(test$Survived)
