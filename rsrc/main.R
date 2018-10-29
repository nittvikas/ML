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