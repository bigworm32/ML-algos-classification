library(pracma)
library(mixtools)
library(mvtnorm) 

rm(list=ls())
#set.seed(0.1)
##############################
##kernel functions
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
sigma=2
k5 = function(x,y)
  #gaussian
  return(exp(-sum((x-y)^2)/(2*sigma^2)))
kappa=1
theta=3
k6 = function(x,y)
  return(tanh(kappa*sum(x*y)+theta))
k = function(x,y)
  return(k6(x,y))

#############################
#Random circular data
#############################
data1<- matrix(0,1200,2)

theta<- rnorm(400,0,pi)
r<- rnorm(400,0,.1)
data1[0:400,1] = r*cos(theta)
data1[0:400,2] = r*sin(theta)

theta<- rnorm(400,0,pi)
r<- rnorm(400,2,.1)
data1[401:800,1]<- r*cos(theta)
data1[401:800,2]<- r*sin(theta)

theta<- rnorm(400,0,pi)
r<- rnorm(400,5,.1)
data1[801:1200,1]<- r*cos(theta)
data1[801:1200,2]<- r*sin(theta)

plot(data1)

#########################
#Random linear data
#########################
#set.seed(17)
p <- rmvnorm(1000, c(250000, 20000), matrix(c(100000^2, 22000^2, 22000^2, 6000^2),2,2))
data2 = as.matrix(p)
plot(data2)

###########
#clustered
###########
#set.seed(101)
x=matrix(rnorm(100*2),100,2)
x[1:100,]
plot(x,pch=19)
which=sample(1:3,100,replace=TRUE)
plot(x,col=which,pch=19)
xmean=matrix(rnorm(3*2,sd=4),3,2)
xclusterd=x+xmean[which,]
plot(xclusterd,col=which,pch=19, main = 'Clustered Data')
data3 = xclusterd

################################
#Kernel pca
################################

kpca<- function(data,kernel,reddim){
  #center the matrix
  n = nrow(data)#number of rows
  K = outer(1:n,1:n,Vectorize(function(i,j) kernel(data[i,],data[j,])))#compute the Gramm matrix
  o = matrix(1/n,nrow = n, ncol = n)
  K = K - o%*%K-K%*%o+o%*%K%*%o
  
  #compute eigenvectors and eigevalues
  eigens = eigen(K)
  eval = eigens$values#desecnding order
  evec = eigens$vectors#desending order
  
  #reduce dimensions
  evec = evec[0:n,0:reddim]#reduce dimensions
  evec = matrix(evec,nrow=n,ncol=reddim)#store vectors into a matrix
  eval = eval[0:reddim]#reduce dimensions
  
  #normalize
  alpha = matrix(0,nrow=n,ncol= reddim)
  for(i in 1:length(eval)){
    vi = evec[,i] / sqrt(eval[i])
    alpha[,i] = vi
  }
  #projected eiganvalues
  z = K%*%alpha
  return(z)
}

#call kernels with simulated data
z1 = kpca(data1,k5,2)
par(mfrow=c(2,1))
plot(data1[,1],data1[,2],xlab = '', ylab ='',main = 'Circular Data')
plot(z1[,1],z1[,2],xlab = '', ylab = '', main = 'PCA with Gaussian Kernel')

z2 = kpca(data2,k1,1)
par(mfrow=c(2,1))
plot(data2[,1],data2[,2],xlab = '', ylab = '', main='Linear data')
plot(z2[,1],z2[,1], xlab = '', ylab = '', main = 'PCA with Linear Kernel')

z3 = kpca(data3, k5,2)
#par(mfrow=c(2,1))
plot(data3[,1], data3[,2], xlab = '', ylab = '', main = 'Clustered Data')
plot(z3[,1], z3[,2], xlab = '', ylab = '', main = 'PCA with Gaussian Kernel')











