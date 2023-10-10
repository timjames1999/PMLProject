#Load needed libraries and set seed
library(caret)
library(ggplot2)
library(randomForest)
set.seed(1999)

#Download and read data file once to save time
trainurl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
trainfile <- "training.csv"
testfile  <- "testing.csv"
if (!file.exists("./data")) {
  dir.create("./data")
}
if (!file.exists(trainfile)) {
  download.file(trainurl, destfile=trainfile, method="curl")
}
if (!file.exists(testfile)) {
  download.file(testurl, destfile=testfile, method="curl")
}

train <- read.csv("training.csv", na.strings=c("NA","#DIV/0!",""))
test <- read.csv("testing.csv", na.strings=c("NA","#DIV/0!",""))

#Initial look at data
str(train)
#summary(train)
#featurePlot(x=train[, 150:159], y = train$classe, plot = 'pairs')

#Remove variables with majority NAs (>50%)
train_SUB <- train
for (i in 1:length(train)) {
  if (sum(is.na(train[ , i])) / nrow(train) >= .50) {
    for (j in 1:length(train_SUB)) {
      if (length(grep(names(train[i]), names(train_SUB)[j]))==1) {
        train_SUB <- train_SUB[ , -j]
      }
    }
  }
}

#Remove first 7 columns that are not used as predictors
train_SUB2 <- train_SUB[,8:length(train_SUB)]

#Random Forest Model
modFit <- train(classe ~., method = "rf", trControl=trainControl(method = "cv", number = 4), data = train_SUB2)
print(modFit)

#Prediction on Quiz Test Data
predict_FINAL <- predict(modFit, test, type = "raw")
print(predict_FINAL)