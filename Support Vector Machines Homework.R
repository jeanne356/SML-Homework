###################################
# Support Vector Machine Homework #
###################################


# libraries


install.packages("e1071")
install.packages("ISLR")
install.packages("caTools")

library(e1071)
library(ISLR)
require(caTools)
require(plotrix)



# Exercises 1, 2, 4, 7, and 8. 


# Exercise 1

# Assigning x1 and x2 values
x1 <- -10:10
x2 <- 1 + 3 * x1

# plotting x1 and x2
plot(x1, x2, type = 'l', col='lightblue')
text(c(0), c(-20), "greater than 0", col="lightblue")
text (c(0), c(20), "less than 0", col="lightblue")

# plotting x1 and 1 - x1/2
lines (x1, 1 - x1/2, col="lightcoral")
text(c(0), c(-15), "less than 0", col="lightcoral")
text(c(0), c(15), "greater than 0", col="lightcoral")


# Exercise 2


# Question a

plot(NA,NA, type="n", xlim = c(-4,2), ylim = c(-1,5), asp=1, xlab = "x1", ylab = "x2")
symbols (c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)


# question b

plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")

# question c

plot(c(0, -1, 2, 3), c(0, 1, 2, 8), col = c("blue", "red", "blue", "blue"), 
     type = "p", asp = 1, xlab = "X1", ylab = "X2", ylim=c(-5,4), xlim=c(-5,4))
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)




# Exercise 4


# Creation of a data set with a non linear separation 

set.seed(131)
x = rnorm(100)
y = 3 * x^2 + 4 + rnorm(100)
train = sample(100, 50)
y[train] = y[train] + 3
y[-train] = y[-train] - 3
# Plot using different colors
plot(x[train], y[train], pch="x", lwd=4, col="lightcoral", ylim=c(-4, 20), xlab="X", ylab="Y")
points(x[-train], y[-train], pch="o", lwd=4, col="lightblue")


# fitting a support vector classifier on train

set.seed(315)
z = rep(0, 100)
z[train] = 1
# Take 25 observations each from train and -train
final.train = c(sample(train, 25), sample(setdiff(1:100, train), 25))
data.train = data.frame(x=x[final.train], y=y[final.train], z=as.factor(z[final.train]))
data.test = data.frame(x=x[-final.train], y=y[-final.train], z=as.factor(z[-final.train]))
library(e1071)

svm.linear = svm(z~., data=data.train, kernel="linear", cost=10)
plot(svm.linear, data.train)

# now for the table

table(z[final.train], predict(svm.linear, data.train))

# next step is training SVM with polynomial kernel

set.seed(43656)
svm.poly = svm(z~., data=data.train, kernel="polynomial", cost=10)
plot(svm.poly, data.train)

# the table

table(z[final.train], predict(svm.poly, data.train))


# Last step is training SVM with a radial basis kernel 

set.seed(567)
svm.radial = svm(z~., data=data.train, kernel="radial", gamma=1, cost=10)
plot(svm.radial, data.train)

table(z[final.train], predict(svm.radial, data.train))


# TEST ERROR

plot(svm.linear, data.test)
plot(svm.poly, data.test)
plot(svm.radial, data.test)


# Final tables for linear, polynomial and radial basis kernel 
table(z[-final.train], predict(svm.linear, data.test))
table(z[-final.train], predict(svm.poly, data.test))
table(z[-final.train], predict(svm.radial, data.test))




# Exercise 7 


# question a
var <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
Auto$mpglevel <- as.factor(var)


# question b
set.seed(1)
tune.out <- tune(svm, mpglevel ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)


# question c

# for polynomial 
set.seed(1)
tune.out <- tune(svm, mpglevel ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree = c(2, 3, 4)))
summary(tune.out)

# for polynomial radial: 
set.seed(1)
tune.out <- tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)


# question d

svm.linear <- svm(mpglevel ~ ., data = Auto, kernel = "linear", cost = 1)
svm.poly <- svm(mpglevel ~ ., data = Auto, kernel = "polynomial", cost = 100, degree = 2)
svm.radial <- svm(mpglevel ~ ., data = Auto, kernel = "radial", cost = 100, gamma = 0.01)
plotpairs = function(fit) {
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
    plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
  }
}
plotpairs(svm.linear)


# Classification plots polynomial 
plotpairs(svm.poly)

# Radial classification plots
plotpairs(svm.radial)




# Exercise 8

# question a
set.seed(1)
train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

# question b
svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = 0.01)
summary(svm.linear)

# question c 

train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)

# checking value 
(72 + 65)/(420 + 65 + 72 + 240)


test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)

# checking value 
(15 + 33)/(153 + 15 + 33 + 69)


# question d
set.seed(1)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "linear", ranges = list(cost = 10^seq(-2, 
                                                                                                   1, by = 0.25)))
summary(tune.out)


# question e 
svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)
# checking
(62 + 70)/(423 + 62 + 70 + 245)

test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)

# checking 

(29 + 12)/(156 + 12 + 29 + 73)

# the test error decreases by 15% 


# question f

set.seed(1)
svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial")
summary(svm.radial)

# train
train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
(44 + 77)/(441 + 44 + 77 + 238)

# test
test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)
( 17 + 33)/(151 + 17 + 33 + 69)


# for the radial 
set.seed(1)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "radial", ranges = list(cost = 10^seq(-2, 
                                                                                                   1, by = 0.25)))
summary(tune.out)

#train
svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial", cost = tune.out$best.parameters$cost)
train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
(48+71)/(437+48+71+244)

# test
test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)
(18+30)/(150+18+30+72)


# question g

set.seed(1)
svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2)
summary(svm.poly)

# train
train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)
(110+36)/(449+36+110+205)

# test
test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)
(45+15)/(153+15+45+57)



set.seed(322)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, 
                ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

# train 
svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)
(90+34)/(451+34+90+225)

# test
test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)
(14+41)/(154+14+41+61)
