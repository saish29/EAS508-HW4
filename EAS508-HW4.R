## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
fig.align = 'center'


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Setting the seed and loading the data

library(ISLR2)
set.seed(1)
train <- sample(392,196)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fitting a linear regression on the train data using subset option

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Predicting the estimates for the 392 observations and calculate the MSE for 192 observations 

mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fitting cubic regression and calculating the MSE

lm.fit2 <- lm(mpg ~poly(horsepower, 2), data = Auto, subset = train)

mean((Auto$mpg - predict(lm.fit2, Auto))[-train]^2)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fitting uadratic regression and calculating the MSE

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)

mean((Auto$mpg - predict(lm.fit3, Auto))[-train]^2)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Using different seed and calculatiing the values for all the three regressions - will result into different MSE values.

set.seed(2)
train <- sample(392,196)

# Linear regression MSE 

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2) 

# Cubic regression MSE

lm.fit2 <- lm(mpg ~poly(horsepower, 2), data = Auto, subset = train)

mean((Auto$mpg - predict(lm.fit2, Auto))[-train]^2)

# Quadratic regression MSE


lm.fit3 <- lm(mpg ~poly(horsepower, 3), data = Auto, subset = train)

mean((Auto$mpg - predict(lm.fit3, Auto))[-train]^2)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# LOOCV using glm() package

glm.fit <- glm(mpg ~ horsepower, data = Auto)

coef(glm.fit)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# LOOCV using normal lm() function

lm.fit <- lm(mpg ~ horsepower, data = Auto)

coef(lm.fit)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Cross-validation error using glm() package

library(boot)

glm.fit <- glm(mpg ~ horsepower, data = Auto)

cv.err <- cv.glm(Auto, glm.fit)

cv.err$delta


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Calculating CV error for for polynomial of order 1 to 10 using a for loop.

cv.error <- rep(0,10)

for (i in  1:10) {
  
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
  
}

cv.error


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Calculating k-fold CV error for for polynomial of order 1 to 10 with k = 10

set.seed(17)
cv.error.10 <- rep(0,10)

for (i in  1:10) {
  
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
  
}

cv.error.10



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Creating model matrix for x and storing all salary values in y

# Omitting NA values 

Hitters <- na.omit(Hitters)

x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary

# Creating train and test by setting the R seed

set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Applying PCR to Hitters data to predcit Salary

library(pls)
set.seed(2)
pcr.fit <- pcr(Salary ~., data = Hitters, scale = TRUE, validation = "CV")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Checking summary of our fit

summary(pcr.fit)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Plotting cross-validation MSE 

validationplot(pcr.fit, val.type = "MSEP")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Performing PCR on the training data using subset function and plotting the CV MSE

set.seed(1)

pcr.fit <- pcr(Salary ~., data = Hitters, subset = train, scale = TRUE, 
               validation = "CV")

validationplot(pcr.fit, val.type = "MSEP")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Find the lowest CV error when M = 5


pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 5)

mean((pcr.pred - y.test)^2)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fit PCR on complete dataset using M = 5 identified by CV

pcr.fit <- pcr(y~x, scale = TRUE, ncomp = 5)

summary(pcr.fit)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Implement PLS using plsr() function 

set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, 
                validation = "CV")

summary(pls.fit)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Evaluating the coresponding test set MSE

pls.pred <- predict(pls.fit, x[test, ], ncomp = 1)

mean((pls.pred - y.test)^2)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fitting PLS on complete dataset when M = 1 

pls.fit <- plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 1)

summary(pls.fit)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fit a GAM or predict wage using natural spline functions of years and age.

gam1 <- lm(wage ~ splines::ns(year, 4) + splines::ns(age, 5) + education, data = Wage)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fit the model using smoothing splines

library(gam)

gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Plot the model

par(mfrow = c(1,3))
plot(gam.m3, se = TRUE, col = "blue")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Plotting the GAM created using lm 

par(mfrow = c(1,3))
plot.Gam(gam1, se = TRUE, col = "red")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Performing ANOVA test to determine the best model 

gam.m1 <- gam(wage ~ s(age, 5) + education,  data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)

anova(gam.m1, gam.m2, gam.m3, test = "F")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# summary of gam.m3

summary(gam.m3)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Using the predict method for class GAM

preds <- predict(gam.m2, newdata = Wage)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Using local regression fits in GAM using lo()

gam.lo <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage)

par(mfrow = c(1,3))
plot.Gam(gam.lo, se = TRUE, col = "green")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Using lo() to create interactions before calling gam

gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Plotting the 2D surface using akima package

library(akima)
par(mfrow = c(1,2))
plot(gam.lo.i)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fittinga logistic regression GAM using I() function

gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, 
              family = binomial, data = Wage)

par(mfrow = c(1,3))
plot(gam.lr, se = TRUE, col = "green")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Creeate a table with high earnes in the < HS category

attach(Wage)
table(education, I(wage > 250))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fitting a logsitic regression GAM by skipping the education category

gam.lr.s <- gam( I(wage > 250) ~ year + s(age, df = 5) + education, 
                 family = binomial, subset = (education != "1. < HS Grad"))

par(mfrow = c(1,3))
plot(gam.lr.s, se = TRUE, col = "green")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Generate a simulated data set

set.seed(1)

x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Scatterplot of X against Y. 



myfun <- function(x) {
  x - 2 * x^2
  
}

# Using ggplot2 to plot a scatterplot

library(ggplot2)

ggplot(mapping  = aes(x = x,y = y)) +
  geom_point(colour = "blue") + 
  geom_smooth(method = "lm", formula = "y ~ x + I(x^2)", se = FALSE, 
              colour = "orange")
  
  
# 


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Creating a data frame using random seed

set.seed(25)

data <- data.frame(x = x, y = y)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Computing LOOCV errors from polynomial order 1 to 4. 

loocv.error <- rep(0,4)

for (i in  1:4) {
  
  glm.fit <- glm(y ~ poly(x, i), data = data)
  loocv.error[i] <- cv.glm(data, glm.fit)$delta[1]
  
}

poly_order <- c("Order 1", "Order 2", "Order 3", "Order 4")


setNames(loocv.error, poly_order)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Changing seed and repeating the steps

set.seed(50)

loocv.error <- rep(0,4)

for (i in  1:4) {
  
  glm.fit <- glm(y ~ poly(x, i), data = data)
  loocv.error[i] <- cv.glm(data, glm.fit)$delta[1]
  
}

poly_order <- c("Order 1", "Order 2", "Order 3", "Order 4")


setNames(loocv.error, poly_order)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Looking at coefficients significant for all the order models, can be seen in order 4 polynomial regression

glm.fit4 <- glm(y ~ poly(x, 4), data = data)

summary(glm.fit)




## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Cleaning data and splitting the dataset into train and test data 

# Loading the necessary packages

library(ISLR)
library(glmnet)
library(pls)

# Removing NA values

College <- na.omit(College)


set.seed(49)

samp <- sample(c(TRUE, FALSE), nrow(College), replace = TRUE, prob = c(0.7,0.3))

train <- College[samp, ]
test <- College[!samp, ]

y_test <- test$Apps


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fitting a linear model 

lm.fit <- lm(Apps ~ ., data = train)

# Predicting the values

lm.pred <- predict(lm.fit, test)

lm.mse <- mean((lm.pred - y_test)^2)

lm.mse


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Writing a function to calculate the R^2 value

R_square <- function(y, y_pred) {
  
  y_mean <- mean(y)
  
  rss <- sum((y - y_pred)^2)
  tss <- sum((y - y_mean)^2)
  
  return (1 - (rss/tss))
}

# Calulcation R^2 value for Linear Model 

lm.r2 <- R_square(y_test, lm.pred)

lm.r2


## ---- fig.align='center'----------------------------------------------------------------------------------------------------------------------------------------------

# Ridge Regression Model 

library(glmnet)

train_mat <- model.matrix(train$Apps ~ ., data = train)

y_train <- train$Apps

test_mat <- model.matrix(test$Apps ~ ., data = test)

y_testR <- test$Apps

set.seed(20)

#ridge.fit <- glmnet(train_mat, y_train, alpha = 0)

par(mfrow = c(1,1))

ridge.cv <- cv.glmnet(train_mat, y_train, alpha = 0)

plot(ridge.cv)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Finding the optimal lambda value and fitting the model again using it

opt_lambda <- ridge.cv$lambda.min

ridge.fit <- glmnet(train_mat, y_train, lambda = opt_lambda, alpha = 0)

ridge.fit


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Predicting the values using ridge regression and finding the the test MSE

ridge.pred <- predict(ridge.fit, newx = test_mat, s = opt_lambda)

ridge_mse <- mean((ridge.pred - y_testR)^2)

ridge_mse


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Calculating R^2 for Ridge regression 

ridge.r2 <- R_square(y_testR, ridge.pred)

ridge.r2


## ---- fig.align='center'----------------------------------------------------------------------------------------------------------------------------------------------

# Fitting Lasso by setting alpha = 1 in glmnet

set.seed(20)

lasso.cv <- cv.glmnet(train_mat, y_train, alpha = 1)

plot(lasso.cv)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Finding the optimnal lamda value and fitting the lasso model again using that value. 

lasso_lamda <- lasso.cv$lambda.min

lasso_lamda

lasso.fit <- glmnet(train_mat, y_train, lambda = lasso_lamda, alpha = 1)

lasso.fit


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Predicting values using the optimal lambda and finding the test MSE

lasso.pred <- predict(lasso.fit, newx = test_mat, s = lasso_lamda)

lasso_mse <- mean((lasso.pred - y_testR)^2)

lasso_mse


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# calculation R^2 for Lasso Model

lasso.r2 <- R_square(y_testR, lasso.pred)

lasso.r2


## ---- fig.align='center'----------------------------------------------------------------------------------------------------------------------------------------------

# Fitting PCR mode using pcr

set.seed(20)

pcr.fit <- pcr(Apps ~ ., data = train, scale = TRUE,  validation = "CV")

# Checking for M value by using validation point

validationplot(pcr.fit, val.type = "MSEP", type = "b", col = "orange", pch = 15)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Predicting the values with ncomp = 17

pcr.pred <- predict(pcr.fit, test, ncomp = 17)

pcr_mse <- mean((pcr.pred - test$Apps)^2)

pcr_mse


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Calculating R^2 value for PCR 

pcr.r2 <- R_square(y_test, pcr.pred)

pcr.r2


## ---- fig.align='center'----------------------------------------------------------------------------------------------------------------------------------------------

# Fitting the PLS model 

pls.fit <- plsr(Apps ~ ., data = train, scale = TRUE, validation = "CV")


validationplot(pls.fit, val.type = "MSEP", type = "b", col = "blue", pch = 15)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Predicting the values by using ncomp as 13

pls.pred <- predict(pls.fit, test, ncomp = 13)

pls_mse <- mean((pls.pred - test$Apps)^2)

pls_mse


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Calculating R^2 value for PLS

pls.r2 <- R_square(y_test, pls.pred)

pls.r2


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Merging all the MSEs in a data frame

cod <- data.frame(method = c("Linear", "Ridge", "Lasso", "PCR", "PLS"), test.MSE = c(lm.mse, ridge_mse, lasso_mse, pcr_mse, pls_mse), RSquared = c(lm.r2, ridge.r2, lasso.r2, pcr.r2, pls.r2))

cod


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Cleaning data and splitting the dataset into train and test data 

# Loading the necessary packages

library(ISLR)
library(glmnet)
library(gam)
library(leaps)

# Removing NA values

College <- na.omit(College)


set.seed(120)

samp <- sample(c(TRUE, FALSE), nrow(College), replace = TRUE, prob = c(0.7,0.3))

train <- College[samp, ]
test <- College[!samp, ]




## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Performing forward stepwise selection with out of state tution as the response and other variables as predictors 

sub.for <- regsubsets(Outstate ~ ., data = train, nvmax = ncol(College)-1, method = "forward")

sum.for <- summary(sub.for)

# Finding minimum values for Cp and BIC and maximum value for adjusted R2

cp.for <- which.min(sum.for$cp)
bic.for <- which.min(sum.for$bic)
ar2.for <- which.max(sum.for$adjr2)

# Plotting using in built plot function of regsubsets()

par(mfrow = c(1,3))

plot(sum.for$bic,xlab="Number of variables",ylab= "BIC value",type="b")

points(bic.for,sum.for$bic[bic.for],col="blue", cex = 2, pch = 20)

plot(sum.for$cp, xlab="Number of variables", ylab= "CP value",type="b", main = "Forward Selection")

points(cp.for, sum.for$cp[cp.for], col="blue", cex = 2, pch = 20)

plot(sum.for$adjr2, xlab="Number of variables", ylab= "Adjusted RS",type="b")

points(ar2.for, sum.for$adjr2[ar2.for], col="blue", cex = 2, pch = 20)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# See coeeficient instances for the best 11-variable models identified by our forward selection 

for.co <- coef(sub.for, 6)

names(for.co)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fitting a GAM using the predictors selected from forward step 

gam.fit <- gam(Outstate ~ Private + s(Room.Board, df = 2) + 
                 s(PhD, df = 2) + s(perc.alumni, df = 2) + s(Expend, df = 2) + 
                 s(Grad.Rate, df = 2), data = train)

par(mfrow = c(2,3))

plot(gam.fit, se = T, col = "blue")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Predicting the values by passing the test data set

pred.gam <- predict(gam.fit, test)

# Calculating test MSE, RMSE and R2 values 

gam.mse <- mean((pred.gam - test$Outstate)^2)

cat("TEST MSE :", gam.mse, "\n")

gam.rmse <- sqrt(gam.mse)

cat("TEST RMSE :", gam.rmse, "\n")

gam.r2 <- R_square(test$Outstate, pred.gam)

cat("R squared :", gam.r2, "\n")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Linear model using the 6 predictors 

lm.pred1 <- predict(lm(Outstate ~ Private + Room.Board + PhD + perc.alumni + 
                         Expend + Grad.Rate, data = train), test)

lm.mse <- mean((lm.pred1 - test$Outstate)^2)

cat("TEST MSE :", lm.mse, "\n")

lm.rmse <- sqrt(lm.mse)

cat("TEST RMSE :", lm.rmse, "\n")

lm1.r2 <- R_square(test$Outstate, lm.pred1)

cat("R Squared :", lm1.r2, "\n")


