setwd("C:/Users/Graham/Documents/Machine Learning")
library(tidyverse)
library(caret)

training <- read_csv("pml-training.csv")
#removing columns with significant numbers of NA values.
#every column with more than 1% NA values had more than 90%
#NA values so 1% as a cutoff didn't remove informative columns
training$classe %>% is.na %>% sum %>% print
((training %>% is.na %>% colMeans) > 0) %>% sum %>% print
((training %>% is.na %>% colMeans) > 0.01) %>% sum %>% print
training <- training %>% select(which(colMeans(is.na(.)) < .01))

#removing one row with remaining NA values and the metadata columns
#like timestamps at the far right of the data frame
which(rowMeans(is.na(training)) > 0)
which(is.na(training[which(rowMeans(is.na(training)) > 0),]) >  0)
training <- training %>% filter(rowMeans(is.na(training)) == 0)
training <- training %>% select(-(1:7))

set.seed(365)
rpfit <- train(classe ~ ., data = training, method = "rpart")
rpfit2 <- train(classe ~ ., data = training, method = "rpart", trControl = trainControl("cv", 20))
rpfit3 <- train(classe ~ ., data = training, method = "rpart", trControl = trainControl("cv", 200))
rpfit4 <- train(classe ~ ., data = training, method = "rpart", trControl = trainControl("boot", 20))
rpfit5 <- train(classe ~ ., data = training, method = "rpart", trControl = trainControl("boot", 200))
print(mean(predict(rpfit4) == training$classe))

set.seed(24)
kmfit <- kmeans(x = (training %>% select(-classe)), centers = 5, iter.max = 5000, nstart = 2000)

set.seed(60)
ldfit <- train(classe ~ ., data = training, method = "lda", trControl = trainControl(method = "cv", number = 20))
print(mean(predict(ldfit) == training$classe))

set.seed(7)
rffit <- train(classe ~ ., data = training, method = "ranger", trControl = trainControl(method = "cv", number = 12))