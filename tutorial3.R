# Set working directory and import datafiles
setwd("Y:/RStudio")
train <- read.csv("Y:/RStudio/train.csv") #train <- read.csv("train.csv", stringsAsFactors=FALSE)    -stops import defaulting test to factors
test <- read.csv("Y:/RStudio/test.csv")

# Install and load required packages for fancy decision tree plotting
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Recreate the gender model
fit <- rpart(Survived ~ Sex, data=train, method="class")
fancyRpartPlot(fit)

# Build a deeper tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
# Plot it with base-R
plot(fit)
text(fit)
# And then make it look better with fancyRpartPlot!
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirsttree.csv", row.names = FALSE)

?rpart.control

# Let's unleash the decision tree and let it grow to the max
#example of overfitting
#Overfitting is technically defined as a model that performs better on a training set than another simpler model, but does worse on unseen data, as we saw here. 
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfullgrowntree.csv", row.names = FALSE)

#have a play about with various control parameters and create interactive version
#Manually trim a decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control(minsplit=2, cp=0))
new.fit <- prp(fit,snip=TRUE)$obj  ####interactive controls
fancyRpartPlot(new.fit)













