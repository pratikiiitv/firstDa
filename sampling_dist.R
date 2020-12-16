#
# DEMO1: sampling distribution of sample mean and sample variances
# Objective: to observe the bias and consistency of estimators
#
# Author: Pratik Shah
# Place: IIITV, Gandhinagar
# Date: Aug, 2019
# Reference: Michael Baron, Probability and Statistics, Chapter-8
#
# Given that the population is Guassianly distributed (truncated version)
# with mean 9 and standard deviation of 0.1, sample the population with
# n=10,100 and 1000 by repeating the experiment m=10^4 times for given n
# check for bias and consistency in the estimates.


library(extraDistr)
mu<-9; s<-.1; a<-8; b<-10;
m<-10^4; 
x<-matrix(,m,ncol=0);
y<-matrix(,m,ncol=0);
y1<-matrix(,m,ncol=0);

br<-seq(a,b,length=100)
cr<-seq(0,.05,length=100)

par(mfrow=c(3,3))

for(k in 1:3){
n<-10^k;
X<-matrix(rtnorm(m*n,mu,s,a,b),m,n);
#X<-matrix(rnorm(m*n,mu,s),m,n);
tmp<-rowSums(X)/n;
x<-cbind(x,tmp);
y<-cbind(y,rowSums((X-tmp)^2)/n);
y1<-cbind(y1, rowSums((X-tmp)^2)/(n-1))
hist(x[,k],breaks=br)
abline(v=mean(x[,k]),col="blue",lwd=1)
abline(v=mu,col="red",lwd=1)
hist(y[,k],breaks=cr)
abline(v=mean(y[,k]),col="blue",lwd=1)
abline(v=s^2,col="red",lwd=1)
hist(y1[,k],breaks=cr)
abline(v=mean(y1[,k]),col="blue",lwd=1)
abline(v=s^2,col="red",lwd=1)
}




