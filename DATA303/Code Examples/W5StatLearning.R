### 4.1 Example 1: Training MSE and test MSE

### Simulating training data
set.seed(123)
x <- runif(10,0,10)
e <- rnorm(10)
y = 5+ 2*x + e

train<- data.frame(x,y)
train
plot(x,y)


### Fit a linear regression
trained_mod <- lm(y ~ x, data = train)
beta_hat=coef(trained_mod)
beta_hat



### Calculate predicted value on the training set
y_hat = predict(trained_mod, data = train)
y_hat



### Calculate the training MSE
#training MSE(mean square error)
trainMSE=mean((train$y-y_hat)^2) 
trainMSE


### Simulating test data  
set.seed(0)
x <- runif(10,0,10)
e <- rnorm(10)
y = 5+ 2*x + e

test<- data.frame(x,y)
test


### Calculate predicted value on test set
y_hat_test = predict(trained_mod, data = test)
y_hat_test

### Calculate the test MSE
#test MSE(mean square error)
testMSE=mean((test$y-y_hat_test)^2) 
testMSE








### 4.2 Example2: Overfitting and underfitting 

### Auto data set
library(ISLR)
head(Auto) # Print the first 6 observations
length(Auto$mpg) # This give us the number of observations in 'Auto' data set

plot(Auto$horsepower, Auto$mpg)
abline(lm(mpg ~ horsepower, data = Auto))


### Fit polynomial regression models
# Fit the linear model
fit1 <- lm(mpg ~ horsepower, data = Auto) 
# Average of squared error on the test set
trainMSE1 <- mean((Auto$mpg - predict(fit1, Auto))^2) 
trainMSE1


# Fit the  polynomial regression of degree 2 
fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto)  

# Average of squared error on the test set
trainMSE2 <- mean((Auto$mpg - predict(fit2, Auto))^2) 
trainMSE2


# Fit the  polynomial regression of degree 3 
fit10 <- lm(mpg ~ poly(horsepower, 10), data = Auto)  

# Average of squared error on the test set
trainMSE10 <- mean((Auto$mpg - predict(fit10, Auto))^2) 
trainMSE10


library(ggplot2)
ggplot(Auto, aes(x=horsepower, y=mpg)) + 
  geom_point()+
  geom_line(aes(x=horsepower,y=predict(fit1, Auto)), color="blue", lwd=1.2)+
  geom_line(aes(x=horsepower,y=predict(fit2, Auto)), color="red", lwd=1.2)+
  geom_line(aes(x=horsepower,y=predict(fit10, Auto)), color="yellow", lwd=1.2)






### 4.3 Example3: Training MSE and test MSE

### Simulating  training data
set.seed(123)
x <- runif(50,-10,10)
e <- rnorm(50, 0, 200)
y = 5+ 2*x + 2*x^2 + 2*x^3+ e

train<- data.frame(x,y)
plot(x,y)



### Simulating  test data
set.seed(1)
x <- runif(50,-10,10)
e <- rnorm(50, 0, 200)
y = 5+ 2*x + 2*x^2 + 2*x^3+ e

test<- data.frame(x,y)
#test



### Fit polynomial regression models and calculate the training MSE and test MSE
trainMSE = rep(0,10)
testMSE = rep(0,10)
for (i in 1:10){
  #Training the model
  train_mod <- lm(y ~ poly(x, i), train)
  
  #training MSE(mean square error)
  trainMSE[i]=mean((train$y-predict(train_mod, train))^2)
  
  #test MSE(mean square error)
  testMSE[i]=mean((test$y-predict(train_mod, test))^2)
}


#Create MSE plot
mse = rbind(data.frame(Train_Test="TrainMSE", MSE=trainMSE, degree=c(1:10)),
            data.frame(Train_Test="testMSE", MSE=testMSE, degree=c(1:10) ) )


ggplot(mse, aes(x=degree, y=MSE)) + geom_line(aes(linetype=Train_Test))







