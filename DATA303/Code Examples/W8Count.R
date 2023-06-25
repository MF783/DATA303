## 1. Simple example

# Create data frame for th esimple example
df <- data.frame(Events=c(3,3,3), Group=c('a','b','c'), Years=c(1,2,3))
df

# Fit the Poisson regression `Events~Group`  with offset log(Years)
m0 <- glm(Events~factor(Group)+offset(log(Years)), family=poisson, data=df)
summary(m0)


# Estimates of beta_0, beta_1, beta_2 from the Poisson regression
m0$coefficients

# Calculation of rates
rates <- c(exp(m0$coefficients[1]),
           exp(m0$coefficients[1]+m0$coefficients[2]),
           exp(m0$coefficients[1]+m0$coefficients[3])) 
names(rates) <- c("theta_a","theta_b","theta_c")
rates



## 2. Simple example 2 (Smoking and coronary death)
#read data set
df <- read.csv("Poisson.csv")
attach(df)
df
dim(df)

### Fit the Poisson regression `deaths~agecat + smokecat`  with offset log(person.years)
m1 <- glm(deaths~agecat+factor(smokecat)+offset(log(person.years)), family=poisson, data=df)
summary(m1)


# Rate ratio
beta_hat <- summary(m1)$coefficients[,1]
beta_hat
exp(beta_hat[3])

#95% confidence interval
#SE_beta_hat <- summary(m1)$coefficients[,2]
#SE_beta_hat

#exp( c(beta_hat[3] - 1.96 * SE_beta_hat[3], 
#       beta_hat[3] + 1.96 * SE_beta_hat[3]) )


exp(confint(m1))[3,]

































































































