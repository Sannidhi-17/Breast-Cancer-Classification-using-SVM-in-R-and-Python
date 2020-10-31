
# import the libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(corrr)
library(kernlab)
library(e1071)
library(DT)

# Import the data set
cancer_data <- read.csv('data.csv')
cancer_data %>% datatable(caption = "Cancer Data")
glimpse(cancer_data)

# Checking the data is null or not
colSums(is.na(cancer_data))

cancer_data <- select(cancer_data, -X)

# Data Visualization
corrdata <- cancer_data[,-c(1,2)]
corrplot(cor(corrdata), order = 'hclust')

# distribution of benign vs malign
print(ggplot(data = cancer_data, aes(x = diagnosis, fill = diagnosis)) + geom_bar())

# Split the data set into train and test
library(caTools)
set.seed(10)
ind.train <- createDataPartition(cancer_data$diagnosis, p=0.8, list=FALSE)
cancer_data_train <- cancer_data[ind.train,]
cancer_data_test <- cancer_data[-ind.train,]

# Create the linear model
cost_range <-c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 1.5, 2, 5)

tune.out <- tune(svm, diagnosis~. -id, data = cancer_data_train, kernel = "linear",
                 ranges = list(cost=cost_range))

bestmod_linear <- tune.out$best.model
summary(bestmod_linear)

predictions_train <- predict(bestmod_linear)
confusionMatrix(predictions_train, cancer_data_train$diagnosis)