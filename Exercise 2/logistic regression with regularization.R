#Logistic regression
setwd('D:/ml-class/Programming Exercises/mlclass-ex2/mlclass-ex2')
data=read.csv('ex2data2.csv',sep=',')
names(data)=c("X1","X2","X3")
attach(data)
X=as.matrix(cbind(rep(1,117),X1,X2))
Y=function(X1,X2){
  Z=c()
   for (i in (seq(0,6,1))){
    for (j in (seq(0,(6-i),1))){
      Z=c(Z,X1^i * X2^j)
    }
   }
   K=matrix(Z,nrow=117,byrow=FALSE)
  return (K)}
l=Y(X1,X2)
weight=matrix(rep(0,28))
H=l%*%weight
H1=1/(1+exp(-H))
newcost=0
cost=1/117*sum(-X3*log(H1)-(1-X3)*log(1-H1))+ 1/(2*117)*sum(weight[1:28,]^2)
J=c()
#C=c()
D=c()
while(newcost<=cost)
{cost=1/117*sum(-X3*log(H1)-(1-X3)*log(1-H1))+ 1/(2*117)*sum(weight[1:28,]^2)
 change=(0.01/117)*(t(l)%*%(H1-matrix(X3)) + 1*weight)
 D=c(D,weight[8,1])
 weight=weight-change
 J=c(J,cost)
 #C=c(C,H1)
 H=l%*%weight
 H1=1/(1+exp(-H))
 newcost=1/117*sum(-X3*log(H1)-(1-X3)*log(1-H1))+ 1/(2*117)*sum(weight[1:28,]^2)}
weight
J
#C
plot(J~seq(1:length(J)))
#change
#jpeg("Convergence Graph-Multivariate regression.jpg")
#plot(J~seq(1:length(J)),xlab="No of iterations",ylab="Cost Function",main="Convergance graph",col="red")
#dev.off()"""
#summary(glm(Admit~.,data=data,family=binomial))
