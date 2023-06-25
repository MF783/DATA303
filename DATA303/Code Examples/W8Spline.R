##  GAM (Generalized Additive Model)

###  Example (Wage data)
library(ISLR2)
str(data.frame(Wage$wage, Wage$year, Wage$age, Wage$education))



##GAM
library(splines)
library(gam)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data=Wage)
par(mfrow=c(1,3))
plot.Gam(gam.m3, se=TRUE, col="red")



#Compare m1, m2, m3 using anove() function and AIC
library(ISLR2)
gam.m1 <- gam(wage ~  s(age, 5) + education, data=Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data=Wage)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data=Wage)

anova(gam.m1, gam.m2, gam.m3, test="F")


AIC(gam.m1, gam.m2, gam.m3)


# the best model
summary(gam.m2)




###  Example 3 (Model selection by  test MSE)
set.seed(1)

dim(Wage)

train_index <- sample(1:dim(Wage)[1], dim(Wage)[1]*0.7)
train <- Wage[train_index,]
test <- Wage[-train_index,]
dim(train)
dim(test)

# traing the models with the training set
gam.m1 <- gam(wage ~  s(age, 5) + education, data=train)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data=train)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data=train)

# Calculate the test MSE wth the test set
mse1 <- mean((test$wage - predict(gam.m1, test))^2)
mse2 <- mean((test$wage - predict(gam.m2, test))^2)
mse3 <- mean((test$wage - predict(gam.m3, test))^2)

c(mse1, mse2, mse3)







