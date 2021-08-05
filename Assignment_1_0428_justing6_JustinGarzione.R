#Name: Justin Garzione
#NetID: justing6
#UIN: 656850428

library(class)
library(ggplot2)
set.seed(428)

#Step 1: Generate a training sample of size 200, and a test sample of size 10,000
csize = 10; #Number of centers
p = 2;
sigma = 1; #Standard Deviation for generating the centers
m1 = matrix(rnorm(csize*p),csize,p)*sigma + cbind(rep(1,csize), rep(0,csize))
m0 = matrix(rnorm(csize*p),csize,p)*sigma + cbind(rep(0,csize), rep(1,csize))

#Step 2: Calculate the training and test errors (the averaged 0/1 error)
n = 100;

#Randomly allocate the n samples for class 1 to the 10 clusters
id1 = sample(1:csize, n, replace=TRUE);

#Randomly allocate the n samples for class 1 to the 10 clusters
id0 = sample(1:csize, n, replace=TRUE);

s = sqrt(1/5); #standard deviation for generating x

traindata = matrix(rnorm(2*n*p), 2*n, p)*s + rbind(m1[id1,], m0[id0,])
Ytrain = factor(c(rep(1,n), rep(0,n)))

N = 5000
id1 = sample(1:csize, N, replace=TRUE);
id0 = sample(1:csize, N, replace=TRUE);
testdate = matrix(rnorm(2*N*p), 2*N, p)*s + rbind(m1[id1,], m0[id0,])
Ytest = factor(c(rep(1,N), rep(0,N)))


#Step 3: For Each do the following steps:

#--Step 4: Calculate Linear regression with cut-off value 0.5

#--Step 5: Calculate Quadratic regression with cut-off value 0.5

#--Step 6: Calculate kNN classification with k chosen by 10-fold cross validation

#--Step 7: Calculate Bayes rule

#Step 8: Summarize your results on training errors and test errors graphically; boxplot or stripchart
plot(traindata[, 1], traindata[, 2], type = "n", xlab = "", ylab = "")

points(traindata[1:n, 1], traindata[1:n, 2], col = "blue");
points(traindata[(n+1):(2*n), 1], traindata[(n+1):(2*n), 2], col="red"); 

points(m1[1:csize, 1], m1[1:csize, 2], pch="+", cex=1.5, col="blue");    
points(m0[1:csize, 1], m0[1:csize, 2], pch="+", cex=1.5, col="red");   

legend("bottomleft", pch = c(1,1), col = c("red", "blue"), legend = c("class 1", "class 0"))

#Step 9: Report the mean and standard errors for the k values

