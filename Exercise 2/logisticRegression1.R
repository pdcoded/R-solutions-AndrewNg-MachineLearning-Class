#Logistic regression
setwd('D:/ml-class/Programming Exercises/mlclass-ex2/mlclass-ex2')
data=read.csv('ex2data1.csv',sep=',')
names(data)=c("Exam1","Exam2","Admit")
attach(data)
X=as.matrix(cbind(rep(1,99),Exam1,Exam2))
weight=matrix(c(0,0,0))
H=X%*%weight
H1=1/(1+exp(-H))
newcost=0
cost=1/99*sum(-Admit*log(H1)-(1-Admit)*log(1-H1))
J=c()
while(newcost<=cost)
{cost=1/99*sum(-Admit*log(H1)-(1-Admit)*log(1-H1))
 change=(0.00095/99)*(t(X)%*%(H1-matrix(Admit)))
 weight=weight-change
 H=X%*%weight
 H1=1/(1+exp(-H))
 J=c(J,cost)
 newcost=1/99*sum(-Admit*log(H1)-(1-Admit)*log(1-H1))}
weight
J
change
#jpeg("Convergence Graph-Multivariate regression.jpg")
#plot(J~seq(1:length(J)),xlab="No of iterations",ylab="Cost Function",main="Convergance graph",col="red")
#dev.off()"""
summary(glm(Admit~.,data=data,family=binomial))
