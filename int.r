library(caret)
filename <- "D:/DS/bank/bank-full.csv"

dataset <- read.csv(filename, header=TRUE, sep = ";")
dataframe = as.data.frame(dataset)

validation_index <- createDataPartition(dataframe$y, p=0.80, list=FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]
percentage <- prop.table(table(dataframe$y))

x <- dataset[,1:16]
y <- dataset[,17]

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

set.seed(100)
fit_lda <- train(y~., data=dataset, method="lda", metric=metric, trControl=control)

predictions <- predict(fit_lda, validation)
confusionMatrix(predictions, validation$y)

duration_per_campaign = dataframe$duration / dataframe$campaign

opt <- function(x){
  return(x)
}

optimized_value <- optimize(opt, interval = duration_per_campaign, tol = .Machine$double.xmin^0.5)
