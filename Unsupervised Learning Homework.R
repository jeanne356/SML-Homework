# Unsupervised Learning Exercises

# Exercises 3, 8, 9 and 10



# Libraries
library(ISLR)


# Exercise 3 


set.seed(1)
x = cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))
x

# question a

plot(x[,1], x[,2])

# question b

labels = sample(2, nrow(x), replace=T)
labels

# question c

centroid1 = c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
centroid2 = c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
centroid1

centroid2

plot(x[,1], x[,2], col=(labels+1), pch=20, cex=2)
points(centroid1[1], centroid1[2], col=2, pch=4)
points(centroid2[1], centroid2[2], col=3, pch=4)


# question d

labels <- c(1, 1, 1, 2, 2, 2)
plot(x[, 1], x[, 2], col = (labels + 1), pch = 20, cex = 2)
points(centroid1[1], centroid1[2], col = 2, pch = 4)
points(centroid2[1], centroid2[2], col = 3, pch = 4)




# Exercise 8

set.seed(1)


# question a 

pr.out = prcomp(USArrests, center=T, scale=T)
pr.var = pr.out$sdev^2
pve = pr.var / sum(pr.var)
pve

# question b

loadings <- pr.out$rotation
USArrests2 <- scale(USArrests)
sumvar <- sum(apply(as.matrix(USArrests2)^2, 2, sum))
apply((as.matrix(USArrests2) %*% loadings)^2, 2, sum) / sumvar



# Exercise 9

# a.
set.seed(2)
hc.complete <- hclust(dist(USArrests), method = "complete")
plot(hc.complete)

# b.

cutree(hc.complete, 3)


# c.

sd.data <- scale(USArrests)
hc.complete.sd <- hclust(dist(sd.data), method = "complete")
plot(hc.complete.sd)


# d.

cutree(hc.complete.sd, 3)



# Exercise 10


# a.
set.seed(2)
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1


# b.
pca.out = prcomp(x)
summary(pca.out)

# Keeping 2 columns
pca.out$x[,1:2]

# plotting 
plot(pca.out$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=19) 


# c.

km.out = kmeans(x, 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

# d.

km.out = kmeans(x, 2, nstart=20)
km.out$cluster

# e.

km.out = kmeans(x, 4, nstart=20)
km.out$cluster

# f
km.out = kmeans(pca.out$x[,1:2], 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

# g

km.out = kmeans(scale(x), 3, nstart=20)
km.out$cluster
