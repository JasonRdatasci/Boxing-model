library(tidyr)
library(dplyr)
library(caTools)
library(nnet)

boxing <- read.csv('bouts_out_new.csv')

#deletes judged scores because we don't want to include judged scores when evaluating if a boxer would win based on their stats
boxing <- boxing[,-(21:26)]

#deletes any NA values - deletes a lot of rows but still has about 7,000 observations to use
boxing <- na.omit(boxing)

##change the minimum and maximum ages, heights, reaches, and weights based on realistic minimums and maximums
boxing <- subset(boxing, age_A >= 16 & age_A <= 65)
boxing <- subset(boxing, age_B >= 16 & age_B <= 65)
boxing <- subset(boxing, height_A >= 147 & height_A <= 214)
boxing <- subset(boxing, height_B >= 147 & height_B <= 214)
boxing <- subset(boxing, reach_A >= 160 & reach_A <= 214)
boxing <- subset(boxing, reach_B >= 160 & reach_B <= 214)
boxing <- subset(boxing, weight_A >= 95 & weight_A <= 323)
boxing <- subset(boxing, weight_B >= 95 & weight_B <= 323)

##set sample and train/test split
set.seed(101)
sample <- sample.split(boxing$result, SplitRatio = 8)
boxing_train <- subset(boxing, sample == TRUE)
boxing_test <- subset(boxing, sample == FALSE)


#using a multinomial model to train the dat with
model <- multinom(result ~ .-decision, data = boxing_train)

#using the model to test the testing data on
predict_result <- predict(model, boxing_test)

#confusion matrix
table(predict_result, boxing_test$result)

#misclassification error
mean(as.character(predict_result)!= as.character(boxing_test$result))