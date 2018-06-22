# Set working directory and import datafiles
setwd("Y:/RStudio")
train <- read.csv("Y:/RStudio/train.csv") #train <- read.csv("train.csv", stringsAsFactors=FALSE)    -stops import defaulting test to factors
test <- read.csv("Y:/RStudio/test.csv")

# Install and load required packages for fancy decision tree plotting
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#since the formulas for building a single decision tree are the same every time, 
#some source of randomness is required to make these trees different from one another. 
#Random Forests do this in two ways.

#The first trick is to use bagging, for bootstrap aggregating. Bagging takes a randomized sample of the rows in your training set, with replacement. 
#This is easy to simulate in R using the sample function. Let's say we wanted to perform bagging on a training set with 10 rows.

sample(1:10, replace = TRUE)
#If you run this command again, you will get a different sample of rows each time. 
# Instead of looking at the entire pool of available variables, Random Forests take only a subset of them, typically the square root of the number available.

#Grow a tree on the subset of the data with the age values available, and then replace those that are missing (the NA ages)
summary(combi)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),],
                method="anova")              #use anova because we have a continuous variable not factor (survival)
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi$Embarked)
which(combi$Embarked == '') #identifies the index of the missing "embarked"
combi$Embarked[c(62, 830)] = "S"
combi$Embarked <- factor(combi$Embarked)

summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#Now on to restriction number two: Random Forests in R can only digest factors with up to 32 levels. 
#increase cut-off to be a "Small" family from 2 to 3 people. 
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

str(combi$FamilyID2)

#Okay, we're down to 22 levels so we're good to split the test and train sets back up as we did last lesson and grow a Random Forest. 
#Install and load the package randomForest:
install.packages('randomForest')
library(randomForest)
set.seed(415)      
#set.seed()  is an integer vector, containing the random number generator (RNG) state for random number generation in R. 
#It can be saved and restored, but should not be altered by the user.
#The number inside isn't important, you just need to ensure you use the same seed number each time so that the same random numbers are generated inside the Random Forest function.
#useful for creating simulations or random objects that can be reproduced.

train <- combi[1:891,]
test <- combi[892:1309,]


#force the model to predict our classification by temporarily changing our target variable to a factor with only two levels using as.factor()
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2 + CabCode,
                    data=train,
                    importance=TRUE,    #llows us to inspect variable importance
                    ntree=2000)     #specifies how many trees

#working with a larger dataset you may want to reduce the number of trees, 
#at least for initial exploration, or restrict the complexity of each tree using nodesize as well as reduce the number of rows sampled with sampsize.
#You can also override the default number of variables to choose from with mtry, 
#but the default is the square root of the total number available and that should work just fine.

varImpPlot(fit)

#Remember with bagging how roughly 37% of our rows would be left out? Well Random Forests doesn't just waste those "out-of-bag" (OOB) observations, 
#it uses them to see how well each tree performs on unseen data. It's almost like a bonus test set to determine your model's performance on the fly.
?predict
prediction <- predict(fit, test)
summary(prediction)
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = "firstForest.csv", row.names = FALSE)
#poor performance does go to show that on smaller datasets, sometimes a fancier model won't beat a simple one. 

#more than one ensemble model
#forest of conditional inference trees

#error with installing: due to antivirus
#Debugging the unzip package function and then stepping through it gives the antivirus enough time to do its job without interfering. Use this command:
debug(utils:::unpackPkgZip)
#then install
install.packages('party')
library(party)

set.seed(415)
fit1 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train,
               controls = cforest_unbiased(ntree=2000, mtry=3))
PredictionForest <- predict(fit1, test, OOB=TRUE, type="response")

submitForest <- data.frame(PassengerId = test$PassengerId, Survived = PredictionForest)
write.csv(submitForest, file = "cforest.csv", row.names = FALSE)























