
## Hitters data

library(ISLR2)
names(Hitters)
dim(Hitters)

#`Salary` is missing for $59$ players. 
sum(is.na(Hitters$Salary))

#The `na.omit()` function removes all of the rows 
#that have missing values in any variable.
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))




## Ridge Regression and the Lasso
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary




### Ridge Regression#########################

### Test set apparoach
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]


library(glmnet)
#grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0)
#ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
#                    lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2)



### Cross-validation on training set to find the bset lambda
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam


ridge.pred <- predict(ridge.mod, s = bestlam,
                      newx = x[test, ])
mean((ridge.pred - y.test)^2)


out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]






### LASSO #########################

lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid)
plot(lasso.mod)

### Cross-validation on training set to find the bset lambda
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
log(bestlam)

lasso.pred <- predict(lasso.mod, s = bestlam,
                      newx = x[test, ])
mean((lasso.pred - y.test)^2)



out <- glmnet(x, y, alpha = 1)
#out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
                      s = bestlam)[1:20, ]
lasso.coef
lasso.coef[lasso.coef != 0]











