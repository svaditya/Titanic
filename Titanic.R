# Install Required Packages


#install.packages("caret", dependencies = TRUE)
#install.packages("randomForest")
#install.packages("dplyr")
#install.packages("ggplot2")

library(caret)
library(randomForest)
library(ggplot2)
library(dplyr)


# Reading train and test data from working directory
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

# Checking the intial few records of train and test data

head(train)
head(test)
str(train)

train$Pclass <- as.character(train$Pclass)
train$SibSp <- as.character(train$SibSp)
train$Parch <- as.character(train$Parch)

# Seperating the Features to num and chr
num <- names(train)[sapply(train, is.numeric)]
chr <- names(train)[!(names(train) %in% num)]

# COnvert back to int to be consistant with test data
train$Pclass <- as.integer(train$Pclass)
train$SibSp <- as.integer(train$SibSp)
train$Parch <- as.integer(train$Parch)

# We will exclude some features like PassengerId, Name, Ticket and Cabin.

drop <- c("PassengerId", "Name", "Ticket", "Cabin")
num <- num[!(num %in% drop)]
chr <- chr[!(chr %in% drop)]

train <- train[complete.cases(train[,c(num,chr)]),c(num,chr)]

# Convert the SUrvived variable to factor
train$Survived <- as.factor(train$Survived) 
num <- num[!(num %in% "Survived")]

# Plotting continuous variables against Survived

require(ggplot2)
vis.boxplot <- function(col2, df = train, col1 = "Survived") {
ggplot(df, aes_string(col1, col2)) + geom_boxplot() + ggtitle(paste("Plot of", col1, "Vs", col2))
}
lapply(num, vis.boxplot)

# Plotting categorical variables against Survived

vis.crosstab <- function(col2, df = train, col1 = "Survived") {
round(prop.table(table(df[,col1], df[,col2]),2)*100,1)
#paste("Crosstab of", col1, "Vs", col2)
}

sapply(chr, vis.crosstab)

# Time to train

# Create formula for train function
fmla <- as.formula(paste(" Survived ~", paste(c(num,chr),collapse="+")))

# Set seed for reproducibility
set.seed(123)
trainedmodel <- train(fmla, data = train, method = "rf", trControl = trainControl(method = "cv", number = 5))

# rf denotes the random forest algorithm to be used to train the model
# cv denotes cross validation. The train data is split in to 5 equal parts. The model is trained on 4 parts and tested on 1 part each time. At the end average accuracy is computed by taking mean of accuracies on each test set. CV gives us an assurance that the trained model performs with similar accuracy, if not better, on the out of sample test data.

# Display the trained model
trainedmodel

# Consider only relevant columns
test <- test[,c(num,chr, "PassengerId")]

summary(test)

# Imputing missing values in test data

test$Fare[is.na(test$Fare)] <- median(test$Fare[!is.na(test$Fare)])


trainedmodelage <- train(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = test[!is.na(test$Age),], method = "lm")
trainedmodelage

# Predict the Missing Age values
test[is.na(test$Age), ]$Age <- predict(trainedmodelage, test[is.na(test$Age),])
summary(test)

# FInally predicting the Survived status in the test data

test$Survived <- predict(trainedmodel, newdata = test)
submit <- test[,c("PassengerId", "Survived")]
write.table(submit, file = "submit.csv", col.names = TRUE, row.names = FALSE, sep = ",")

