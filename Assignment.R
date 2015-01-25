remove()
setwd("~/PracticalMacinelearning")
library(caret)
library(foreach)
library(doParallel)
#Read in the data, cleaning it up a little
training<-read.csv("pml-training.csv",na.strings=c("#DIV/0!","NA","") )

summary(training)
#Getting rid of unneeded data

headCleaner <- function(data){
  badCols = grepl("X|user_name|timestamp|new_window", colnames(data))
  
  data <- data[,!badCols]
  data <- data[, (colSums(is.na(data)) == 0)]
  return(data)
  rm(data)
}

training<-headCleaner(training)
#Split the training and testing.

inTrain = createDataPartition(y = training$classe, p = 0.4, list = FALSE)
validating = training[-inTrain, ]
training = training[inTrain, ]

modelFit <- train(classe ~ ., method = "glm",
                  data = training[1:200,])
model <- randomForest(training$classe~.,data=training[1:100,])
summary(training$num_window)
print(modelFit)

confusionMatrix(predict(modelFit,newdata=valid[,-ncol(valid)]),valid$classe)

testing<-read.csv("pml-testing.csv", na.strings=c("#DIV/0!"))

plot(training[1:10])
dev.off

pml_CSV <- read.csv("pml-training.csv", header=TRUE, sep=",", na.strings=c("NA",""))
pml_CSV <- pml_CSV[,-1]
inTrain = createDataPartition(pml_CSV$classe, p=0.60, list=FALSE)
training = pml_CSV[inTrain,]
validating = pml_CSV[-inTrain,]
sum((colSums(!is.na(training[,-ncol(training)])) < 0.6*nrow(training)))
model <- randomForest(classe~.,data=training)
importance(model)
print(model)


confusionMatrix(predict(model,newdata=validating[,-ncol(validating)]),validating$classe)


rf_model<-train(classe~.,data=training,method="rf",
                trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE)
