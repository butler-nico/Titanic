####Titanic: machine learning####
#05/02/2017


# Set working directory and import datafiles
setwd("Y:/RStudio")

train <- read.csv("Y:/RStudio/train.csv") #train <- read.csv("train.csv", stringsAsFactors=FALSE)    -stops import defaulting test to factors
View(train)
test <- read.csv("Y:/RStudio/test.csv")
View(test)

# Examine structure of dataframe
str(train)
train$Survived

# Look at number of people who survived
table(train$Survived)
prop.table(table(train$Survived))

# Create new column in test set with our prediction that everyone dies
test$Survived <- rep(0,418)  

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
