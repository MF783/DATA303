set.seed(7)
## Introduction

#The process of a machine learning project may not be linear, 
#but there are a number of well-known steps:

#* Define Problem.
#* Prepare Data.
#* Evaluate Algorithms.
#* Improve Results.
#* Present Results.


## 1. Load The Data #########################
# attach the iris dataset to the environment
data(iris)
# rename the dataset
dataset <- iris


### 1.1 Create a Validation Dataset
library(caret)
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]




## 2. Summarize Dataset ##########################
### 2.1  Dimensions of Dataset
dim(dataset)

### 2.2 Types of Attributes
sapply(dataset, class)
str(dataset)

### 2.3 Peek at the Data
head(dataset)

### 2.4 Levels of the Class
levels(dataset$Species)

### 2.5 Class Distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

### 2.6 Statistical Summary
summary(dataset)




## 3. Visualize Dataset ###########################
### 3.1 Univariate Plots

# split input and output
x <- dataset[,1:4]
y <- dataset[,5]

# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}
par(mfrow=c(1,1))

# barplot for class breakdown
plot(y)


### 3.2 Multivariate Plots
# scatterplot matrix
#featurePlot(x=x, y=y, plot="ellipse")

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)





## 4. Evaluate Some Algorithms ######################
#Here is what we are going to cover in this step:
  
# * Set-up the test harness to use 10-fold cross validation.
# * Build 5 different models to predict species from flower measurements
# * Select the best model.


### 4.1 Test Harness
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

### 4.2 Build Models
#Let’s evaluate 5 different algorithms:
  
# * Linear Discriminant Analysis (LDA)
# * Classification and Regression Trees (CART).
# * k-Nearest Neighbors (kNN).
# * Support Vector Machines (SVM) with a linear kernel.
# * Random Forest (RF)

# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)



### 4.3 Select Best Model
# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.lda)


## 5. Make Predictions
# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)








