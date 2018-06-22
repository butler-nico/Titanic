# Set working directory and import datafiles
setwd("Y:/RStudio")
train <- read.csv("Y:/RStudio/train.csv") #train <- read.csv("train.csv", stringsAsFactors=FALSE)    -stops import defaulting test to factors
test <- read.csv("Y:/RStudio/test.csv")

# Install and load required packages for fancy decision tree plotting
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train$Name[1]
test$Survived <- NA

#In order to extract these titles to make new variables, we'll need to perform the same actions on both the training and testing set, 
#so that the features are available for growing our decision trees, and making predictions on the unseen testing data.

combi <- rbind(train, test)
summary(combi)

combi$Name <- as.character(combi$Name)
combi$Name[1] 

#regex
strsplit(combi$Name[1], split = '[,.]')
strsplit(combi$Name[1], split = '[,.]')[[1]]  #using the index number
strsplit(combi$Name[1], split = '[,.]')[[1]][2]
combi$Title <- sapply(combi$Name, FUN = function(x){strsplit(x, split = '[,.]')[[1]][2]})
table(combi$Title)
#combi$Title <- strsplit(combi$Name[1], split = '[,.]')[[1]][2]     #using this across whole dataset results in everyone being labled as 'Mr'
combi$Title <- sub(' ', '', combi$Title)   #if all spaces need to be removed and replaced with blanks

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1 #+1 is for persons own existence

#combine surname with family size, perhaps specific familys had more trouble than others getting to lifeboats
combi$Surname <- sapply(combi$Name, FUN=function(x){strsplit(x, split='[,.]')[[1]][1]}) 
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- "Small"
table(combi$FamilyID) #some family sizes have slipped through the cracks and we need to do an additional check to get rid of them
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- "Small"  #overwrite any family IDs in our dataset for groups that were not correctly identified
combi$FamilyID <- factor(combi$FamilyID)

#split the test and training sets back into their original states, carrying the new engineered variables with them
train <- combi[1:891,]
test <- combi[892:1309,]

#New predicton
fit1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train,
             method="class")
fancyRpartPlot(fit1)

#create submission file
Prediction <- predict(fit1, test, type="class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "FeatureEngineering1.csv", row.names = FALSE)

######################################################################################################################
#extra feature engineering
#fields to bring forward - title, familysize, embarked, fare, Pclass, cabin, sex, age
#new fields - CABIN CATEGORY? e.g. C, B, G
substr(combi$Cabin, 1, 1)
table(substr(combi$Cabin, 1, 1))
str(combi)
#new field
combi$CabCode <-substr(combi$Cabin, 1, 1)
combi$CabCode <- factor(combi$CabCode)  #change to factor

train <- combi[1:891,]
test <- combi[892:1309,]

fit2 <- rpart(Survived ~ Title + CabCode + Fare + Pclass + Sex + Age,              #best prediction yet!!  0.80382 accuracy NB: use less variables
              data=train,
              method="class")
fancyRpartPlot(fit2)

#new submission
prediction1 <- predict(fit2, test, type="class")
summary(prediction1)
submit1<-data.frame(PassengerId = test$PassengerId, Survived = prediction1)
write.csv(submit1, file = "FeatureEngineeringCabin.csv", row.names = FALSE)






















