#this introduce python into r

library(reticulate)
library(readr)
library(ggplot2)
#conda_create("pinr")
conda_install("pinr", "pandas")
conda_install("pinr", "numpy")
 

#loading data from the files
data <- read.table("data/Auto.data", header = TRUE, na.strings = "?")
View(data)

#performing sinple linear regression using mpg as the responce and hoursepower as the predictor

names(data)
model_simple = lm(data = data, formula = mpg ~ horsepower)

model_simple
summary(model_simple)
plot(data$horsepower, data$mpg)
abline(model_simple, lwd=3, col='red')
nedata <- data.frame(horsepower = 98)
res_1 = predict(model_simple,newdata =nedata )
res_1
result = predict(model_simple, newdata = data.frame(horsepower= c(98)), interval = "confidence")
result

#dignostic plots for least square regression
par(mfrow = c(2,2))
plot(model_simple)


##multiple linear regression on the Auto data
##
Auto <- read.table("data/Auto.data", header = TRUE, na.strings = "?")

#plo the scatter plot which includes all of variables
plot(Auto)

## Or
## second method
require(ISLR)
data("Auto")
pairs(Auto)

# calculating co-rrelation matrix

cor(subset(Auto, select = -name))

reg_mul_model = lm(mpg~.-name, data = Auto)
reg_mul_model
summary(reg_mul_model)

#diagnostic plot for multi regression plot
plot(reg_mul_model)


#trying interatction
fit.lm0 <- lm(mpg~displacement+weight+year+origin, data=Auto)
fit.lm1 <- lm(mpg~displacement+weight+year*origin, data=Auto)
fit.lm2 <- lm(mpg~displacement+origin+year*weight, data=Auto)
fit.lm3 <- lm(mpg~year+origin+displacement*weight, data=Auto)
summary(fit.lm0)
summary(fit.lm1)
summary(fit.lm2)
summary(fit.lm3)


# some different transformations
z_model_log = lm(mpg~displacement+I(log(year)) ,data = Auto)
z_model_log
summary(z_model_log)

z_model_sqrt = lm(mpg~displacement+I(sqrt(year)), data= Auto)
summary(z_model_sqrt)

z_model_square = lm(mpg~displacement+I(year^2), data=Auto)
summary(z_model_square)


##
## Multiple Regression model
##

require(ISLR)
data("Carseats")

# Predicting sales using price, urban, us
attach(Carseats)
modelsales = lm(Sales ~ Price+Urban+US) 
modelsales
summary(modelsales)


#only those predictors which have the evidance of the relationship with outcome
#
par_model = lm(Sales ~ Price + US)
summary(par_model)

#obtaining the 95% confidence interval for the coefficent
#obtaining the 95% confidence interval
confint(par_model)

#finding the avidence of the liverage or evidence of outliers
par(mfrow=c(2,2))
plot(par_model)

##same question solving 
par(mfrow=c(1,1))
plot(predict(par_model), rstudent(par_model))

#load car packages
require(car)

#cheak are there any outlies in the model
qqPlot(par_model, main = "qqplot")

#leverage plot
leveragePlots(par_model)


## question 11 : 


# t-statistic : Estimating the 
#generating the x and y

set.seed(1)
x =rnorm(100)
y = 2*x+rnorm(100)


#model with out the intercept
mo_not_intercept = lm(y~ x + 0)

#what is the output
mo_not_intercept
summary(mo_not_intercept)
plot(mo_not_intercept)
par(mfrow = c(2,2))


##regressing x on y without the intercept

mo_x_y = lm(x~y+0)
mo_x_y
summary(mo_x_y)


# generating the example in which the coefficient estimation is different for the
# x onto y and y onto x

x = rnorm(100)
mod_xontoy = lm(y~x+0)
mod_yontox = lm(x~y+0)

summary(mod_xontoy)
summary(mod_yontox)

# to get the same coefficient for the both the regression the mean and the standard deviation need
# to be same
set.seed(1)
x = rnorm(100, mean = 1000, sd = 0.1)
y = rnorm(100, mean = 1000, sd = 0.1)

# now we will build the model
mod_same_x = lm(x ~ y)
mod_same_y = lm(y ~ x)
summary(mod_same_x)
summary(mod_same_y)

# Generating the data 
set.seed(11)

x = rnorm(100)

#generating random number between N(0, 0.25)
eps = rnorm(100, sd=0.25^0.5)

#using the above data
# we have to solve the equation Y = -1 +0.5X + (epsolon)
# generating the model 
y = -1 + 0.5 *x +eps

length(y) 

# creating the scatter plot displaying the relationship between x and y
plot(x, y)
# x and y are positively correlated

# fitting the values int the model least square model
xy_model  = lm(y ~ x)
summary(xy_model)
# the intercept and the coefficient are close to which we used in the previous model
par(mfrow=c(1,1))
plot(x,y)
abline(-1, 0.5, col="blue")  # true regression
abline(xy_model, col="red")    # fitted regression
legend(x = c(0,2.7),
       y = c(-2.5,-2),
       legend = c("population", "model fit"),
       col = c("blue","red"), lwd=2 )

## fitting the plynomial line using the X^2
x2y_model = lm(y~x + I(x^2))
summary(x2y_model)
# to check the improvement we can use the anova() function
anova(xy_model, x2y_model)


##question h
##
eps2 <- rnorm(100, sd=0.1)  # prior sd was 0.5
y2 <- -1 + 0.5*x + eps2
fit.lm2 <- lm(y2 ~ x)
summary(fit.lm2)

plot(x, y2)
abline(-1, 0.5, col="blue")   # true regression
abline(fit.lm2, col="red")    # fitted regression
legend(x = c(-2,-0.5),
       y = c(-0.5,0),
       legend = c("population", "model fit"),
       col = c("blue","red"), lwd=2 )


# to completely remove th noice from data we make standard devation to 1

eps3 <- rnorm(100, sd=1)  # orig sd was 0.5
y3 <- -1 + 0.5*x + eps3
fit.lm3 <- lm(y3 ~ x)
summary(fit.lm3)

plot(x, y3)
abline(-1, 0.5, col="blue")   # true regression
abline(fit.lm3, col="red")    # fitted regression
legend(x = c(0,2),
       y = c(-4,-3),
       legend = c("population", "model fit"),
       col = c("blue","red"), lwd=2 )

##
##  14 The concept of the colinearity 
##
set.seed(11)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100) / 10
y = 2 * 2* x1 + 0.3 * x2 + rnorm(100)

cor(x1, x2)
plot(x1, x2)
# plotting the model which will display 
colin_model = lm(y ~ x1 + x2)

colin_model 
summary(colin_model)

# predicting only using x1
mode = lm(y ~ x1)
mode
summary(mode)
