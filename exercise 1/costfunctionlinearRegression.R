#Simple linear regression with one variable 
setwd('D:/ml-class/Programming Exercises/mlclass-ex1/mlclass-ex1')
data=read.csv('ex1data1.csv',sep=',')
names(data)=c("Population","Profit")
attach(data)
"""newcost=0
theta0=0
theta1=0
j=c()
k=c()
l=c()
cost=1/192*sum((theta0+theta1*Population-Profit)^2)
while(newcost<=cost)
{cost=1/192*sum((theta0+theta1*Population-Profit)^2)
 j=c(j,cost)
 k=c(k,theta0)
 l=c(l,theta1)
 temp1=0.01/96*sum((theta0+theta1*Population-Profit)*Population)
 temp0=0.01/96*sum(theta0+theta1*Population-Profit)
 theta1=theta1-temp1
 theta0=theta0-temp0
 newcost=1/192*sum((theta0+theta1*Population-Profit)^2)}
cost
theta0
theta1
library(rgl)  #rgl is a package for making rotating 3d plots. use
#install.packages("rgl") if not installed
plot3d(j,k,l,color="red",size=3)
summary(lm(Profit~Population))
#Values are matching exactly !! Woohoo
plot(j~seq(1:8399))
#this last plot will give u an idea f how many iterations to run and whether the values are decreasing
#Also u can declare convergence when cost - newcost is less that 10^-3 
#or any such convergence rate"""
#Using pure matrix algebra,lets solve it
weight=matrix(c(0,0))
X=as.matrix(cbind(rep(1,96),Population))
H=X%*%weight
newcost=0
cost=1/192*sum((H-matrix(Profit))^2)
while(newcost<=cost)
{cost=1/192*sum((H-matrix(Profit))^2)
 change=(0.01/96)*(t(X)%*%(X%*%weight-matrix(Profit)))
 weight=weight-change
 H=X%*%weight
 newcost=1/192*sum((H-matrix(Profit))^2)}
weight
change
