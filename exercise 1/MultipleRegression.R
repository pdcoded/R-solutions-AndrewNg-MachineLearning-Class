setwd('D:/ml-class/Programming Exercises/mlclass-ex1/mlclass-ex1')
data=read.csv('ex1data2.csv',sep=',')
names(data)=c("size","rooms","Price")
attach(data)
size=(size-mean(size))/sd(size)
rooms=(rooms-mean(rooms))/sd(rooms)
Price=(Price-mean(Price))/sd(Price)
X=as.matrix(cbind(rep(1,46),size,rooms))
weight=matrix(c(0,0,0))
H=X%*%weight
newcost=0
cost=1/92*sum((H-matrix(Price))^2)
J=c()
while(newcost<=cost)
{cost=1/92*sum((H-matrix(Price))^2)
 change=(0.01/46)*(t(X)%*%(X%*%weight-matrix(Price)))
 weight=weight-change
 H=X%*%weight
 J=c(J,cost)
 newcost=1/92*sum((H-matrix(Price))^2)}
weight
J
jpeg("Convergence Graph-Multivariate regression.jpg")
plot(J~seq(1:length(J)),xlab="No of iterations",ylab="Cost Function",main="Convergance graph",col="red")
dev.off()
