# Tree Based Method Exercises


# Exercise 3,5,7, 9 and 10


# Libraries needed :
library(MASS)
library(randomForest)
library(dplyr)
library(ISLR)
library(tree)

# Exercise 3 

# Creation of the plot
# function pm1 in x axis from 0 to 1 
# y axis the value of gini index


p1 = seq(0+1e-5, 1-1e-5, 0.01)
p2 = 1-p1

# Error rate: 
Error = 1 - apply(rbind(p1, p2 - p1),1,max)

# Gini Index
Gini = p1 * (1-p1) + p2 * (1-p2)

# The cross entropy 
Entropy = -(p1 * log(p1) + (1 - p1) * log(1 - p1))

matplot(p1, cbind(Gini, Entropy, Error), col = c("lightpink", "lightblue", "orange"))




# Exercises 5


# Ten oostrapped samples was created from a data set containing red and reen classes
# Classification tree applied to each bosstrapped sample and for a specific value of X we produce 10 estimates of P
# What is the final classification under each of these two approaches ? 

p = c(0.1, 0.15, 0.2, 0.2, 0.55, 0.6, 0.6, 0.65, 0.7,0.75)

# Let's see first with the majority vote approach 

p1= p>=0.5
p2= p<=0.5

sum(p1) > sum(p2)

# Second, let's classify based on the average prbability

# For this we just need the mean of our values
mean(p)


# Exercise 7 

# Creation of the plot that displays the test error 


set.seed(10)

# We applied random forests to the Boston data using mtry=6

BostonData = Boston

sampledata = sample(dim(BostonData)[1], dim(BostonData)[1]/2)
  
traindata= BostonData %>% slice(1:400)
testdata= BostonData %>% slice(401:506)

x.train = BostonData[sampledata, -14]
y.train = BostonData[sampledata, 14]
x.test = BostonData[-sampledata, -14]
y.test = BostonData[-sampledata, 14]


p = 12

RF1 = randomForest(x.train, y.train, x.test, y.test, mtry = p, ntree = 400)
RF2 = randomForest(x.train, y.train, x.test, y.test, mtry = p/2, ntree = 400)
RF3 = randomForest(x.train, y.train, x.test, y.test, mtry = p/3, ntree = 400)
RF4 = randomForest(x.train, y.train, x.test, y.test, mtry = p/4, ntree = 400)


x.axis = seq(1,400,1)
plot(x.axis,RF1$test$mse,xlab = "Number of Trees",ylab="Test MSE", ylim=c(5,20),type="l")
lines(x.axis,RF2$test$mse,col="lightblue")
lines(x.axis,RF3$test$mse,col="lightpink")
lines(x.axis,RF4$test$mse,col="orange")
legend(500,20,legend=c("m=p", "m=p/2", "m=p/3", "m=p/4"),
       col=c("black", "red", "blue", "green"),lty=c(1,1,1), lwd=c(2,2,2))



# Exercise 9: 

set.seed(0)

n = nrow(OJ)
p = ncol(OJ)-1

# Question a
train = sample(1:n,700)
test = (1:n)[-train]

# Question b

OJ.tree = tree(Purchase ~ ., data=OJ[train,])
summary(OJ.tree)

#Question c

cv.OJ = cv.tree( OJ.tree, FUN=prune.misclass )


# Question d 

plot(OJ.tree)
text(OJ.tree)


# Question e

OJ.Prediction = predict(OJ.tree, OJ[test,], type = "class")
table(OJ[test,]$Purchase, OJ.Prediction)


# Question f, g and h 

cv.OJ = cv.tree( OJ.tree, FUN=prune.misclass )

plot(cv.OJ$size, cv.OJ$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")


# Question i and j

OJ.prune <- prune.misclass(OJ.tree, best = 2)
plot(OJ.prune)
text(OJ.prune)
summary(OJ.tree)
summary(OJ.prune)


# question k 

prune.Prediction <- predict(OJ.prune, OJ[test,], type = "class")
table(prune.Prediction, OJ[test,]$Purchase)





