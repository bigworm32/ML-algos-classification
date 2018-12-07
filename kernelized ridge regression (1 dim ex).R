
#hw 2 question 1
#this calculates ridge regression in 1 dimension and outputs a plot kernel and lambda selected from user.  
#user input parameters: change kernel in both the alpha and the ridge regression, change the lambda in bottom function

rm(list=ls())
#set.seed(0.1)
##############################
## A few kernel functions
##############################
k1 = function(x,y)
  return(sum(x*y))

k2 = function(x,y)
  return(sum(x*y)^2)
k3 = function(x,y)
  return((1+sum(x*y))^2)
d=4
k4 = function(x,y)
  return((1+sum(x*y))^d)
sigma=.
k5 = function(x,y)
  return(exp(-sum((x-y)^2)/(2*sigma^2)))
kappa=1
theta=1
k6 = function(x,y)
  return(tanh(kappa*sum(x*y)+theta))
k = function(x,y)
  return(k6(x,y))
#################################
#Generate data in 1 D
#################################
x=seq(from=0,to=10, length.out = 2000)#random x
y = cos(x) + rnorm(2000,0,.6)#randomly develop cosine data
#plot(x,y)

#calculate the alpha
#must chnage from x[i]  to x[i,] when going higher than 1D
alpha.vector<- function(lambda,x,y,n){
  k<- outer(1:n,1:n,Vectorize(function(i,j) k5(x[i],x[j])))#gram matrix, change kernel here-
  lambda.out<-  diag(rep(lambda, times=n))#calculate lambda
  inv.mat<- solve(k + n*lambda.out)#inverse the matrix
  alpha.out<- (inv.mat %*% y)#matrix multiplation on y and (k + lambda*I)
  return(alpha.out)
}


#calculate ridge regression
#must chnage from x[i]  to x[i,] when going higher than 1D
ridge<- function(alpha,x,n){
  y.hat = matrix(NA,nrow=n,ncol = 1)
  for(i in 1:n){
    u<- x[i]
    k.x=outer(1:n,1,Vectorize(function(i,j) k5(x[i],u)))#change function here-
    proj = sum(k.x*alpha)
    y.hat[i,] = proj
  }
  return(y.hat)
}

lambda<- .000001#user change lambda
alpha<- alpha.vector(lambda,x,y,n = length(x))#alpha function
ridge.proj<- ridge(alpha,x, n = length(x))#ridge function
#plot the ridge
plot(x,y,col ='blue')
lines(x,ridge.proj,col='red',lwd=5)
title(main = paste('lambda ', lambda))
