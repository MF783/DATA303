#### Example (Credit data set)

Credit=read.csv("Credit.csv",header=T) 
Credit=Credit[ , -1] # Remove subject ID
head(Credit)         # Print the first 6 records      


### 1.1 Best subset selection
# Perform the best subset selection with 'Balance' as the response 
library(leaps)
regfit.full=regsubsets(Balance~.,Credit)
reg.summary=summary(regfit.full)         # Print summary table


#The output options are given by:
names(reg.summary)


reg.summary$outmat



### 1.2 Choosing the optimal model
#### Plot of C_p, AIC, BIC, and adjusted R^2
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')

#Best model by Cp
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

#print the best subset selected model.
coef(regfit.full,6)
coef(regfit.full,4)




### 1.3 Forward and backward stepwise selection
# Perform forward stepwise selection  
regfit.fwd=regsubsets(Balance~.,Credit,nvmax=19,method="forward") 
reg.summary.fwd=summary(regfit.fwd)


# Perform backward stepwise selection 
regfit.bwd=regsubsets(Balance~.,Credit,nvmax=19,method="backward") 
reg.summary.bwd=summary(regfit.bwd)


par(mfrow=c(1,2))
plot(reg.summary.fwd$cp, main= "Forward selection", 
     xlab="Number of Variables",ylab="Cp",type='l')
plot(reg.summary.bwd$cp, main= "Backward selection", 
     xlab="Number of Variables",ylab="Cp",type='l')


which.min(reg.summary.fwd$cp)
which.min(reg.summary.bwd$cp)












