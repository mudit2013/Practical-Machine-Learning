## read data
training<-read.csv("pml-training.csv",na.strings=c("NA",""))
testing<-read.csv("pml-testing.csv",na.strings=c("NA",""))

## load required libraries
library(caret)

## data cleaning
# cols 1 to 7 (non numeric columns) won't contribute to the model. So remove them.
training   <-training[,-c(1:7)]
# remove NA
training <- training[, which(as.numeric(colSums(is.na(training)))==0)]
# exclude near zero variance features
nearZeroColumns <- nearZeroVar(training, saveMetrics = TRUE)
training <- training[, nearZeroColumns$nzv==FALSE]
#classe into factor
training$classe <- factor(training$classe)

## data partition
set.seed(1234)
trainset <- createDataPartition(training$classe, p = 0.75, list = FALSE)
Training <- training[trainset, ]
Validation <- training[-trainset, ]

## Fit a randomforest classifier
library(randomForest)
modelrf<-randomForest(classe ~ ., data = Training,method="class")
#predict on train set and check the accuracy
predicttraining <- predict(modelrf, Training)
print(confusionMatrix(predicttraining, Training$classe))
# validation set accuracy
predictvalidation <- predict(modelrf, Validation)
print(confusionMatrix(predictvalidation, Validation$classe))
# predict against test set
predicttest <- predict(modelrf, testing)
predicttest
answers <- as.vector(predicttest)

pml_write_files = function(x) {
  n = length(x)
  for (i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE,
                col.names = FALSE)
  }
}

pml_write_files(answers)
