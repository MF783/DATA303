## Example ('Credit' data )

### 1. Preparation: create the responce vector $y$ and the covariate matrix $x$
library(glmnet)
library(ISLR)
#View(Credit) 
head(Credit)
nrow(Credit)

# Create the covariate vector 'x'
# It is the design matrix with the responce variable "Balance" and the rest as  predictors
# [,c(-1,-2)] reves the first and second columns (Intercept and ID)

x=model.matrix(Balance~.,Credit) 
head(x)

x=model.matrix(Balance~.,Credit)[,c(-1,-2)] 
head(x)
dim(x)

# Create the responce variable vector 'y'
y=Credit$Balance
length(y)



###2.  Ridge regression
#Fit the Ridge regression (alpha=0) at each value on 'grid' 
ridge.mod=glmnet(x,y,alpha=0) 
names(ridge.mod)


ridge.mod$lambda

#Regularization paths
plot(ridge.mod, xvar = "lambda", label = TRUE)

#Coefficients

log_lambda=4
lambda=exp(4)

predict(ridge.mod,s=exp(4),type="coefficients")

predict(ridge.mod,s=exp(8),type="coefficients")
predict(ridge.mod,s=exp(12),type="coefficients")



# Least Square Estimate lambda=0
predict(ridge.mod,s=0,type="coefficients")




### 3. Cross validation
set.seed(1)

# Perform 10-fold cross-validation
cv.out=cv.glmnet(x,y,alpha=0)

# Create plot of the test MSE vs log(lambda)
plot(cv.out)

#The value of lambda at the smallest test MSE
names(cv.out)

bestlam=cv.out$lambda.min
bestlam



# log of lambda at the smallest test MSE
log(bestlam)

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)




#User specified gird###########################
grid=10^seq(10,-2,length=100) # Grid: 100 pints from 10^10 to 10^{-2}
grid

# Perform 10-fold cross-validation
cv.out=cv.glmnet(x,y,alpha=0, lambda= grid)

# Create plot of the test MSE vs log(lambda)
plot(cv.out)

#The value of lambda at the smallest test MSE
bestlam=cv.out$lambda.min
bestlam
log(bestlam)

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)


# 1 se rule
lam1se=cv.out$lambda.1se
lam1se 
log(lam1se)

predict(out,type="coefficients",s=lam1se)



### 4. The Lasso

# Fit LASSO (alpha=1) with the training set on the grid
lasso.mod=glmnet(x,y,alpha=1)

lasso.mod$lambda



# Create of plot of coefficients vs log(lambda)
#plot(lasso.mod)
plot(lasso.mod, xvar = "lambda", label = TRUE)

#Coefficients

log_lambda=3
lambda = exp(3)

predict(lasso.mod,s=exp(3),type="coefficients")
predict(lasso.mod,s=exp(3.6),type="coefficients")




# Perform 10-fold cross-validation
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)

#The best value of lambda
bestlam=cv.out$lambda.min
bestlam
log(bestlam)

#The result of Lasso with lambda.min 
out=glmnet(x,y,alpha=1)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef


# 1 se rule
lam1se=cv.out$lambda.1se
lam1se 
log(lam1se)

predict(out,type="coefficients",s=lam1se)





























