
PROGRAM 5


X<-pred2
Y<-pred2_resp


X=as.matrix(X)
Y=as.matrix(Y)

#Normal Regression
W=solve(t(X[1:500,])%*%X[1:500,])%*%(t(X[1:500,])%*%Y[1:500])
Y_pred=X[501:1000,]%*%W
sse=sum((Y_pred-Y[501:1000])^2)

#SSE=32984664


#Ridge Regression
L=20
W=solve((t(X[1:500,])%*%X[1:500,])+(L*diag(500)))%*%(t(X[1:500,])%*%Y[1:500])
Y_pred=X[501:1000,]%*%W
sse1=sum((Y_pred-Y[501:1000])^2)

#SSE1=35918.6

#Performing Ridge Regression on a Range of Values to find the best value of lambda(L)
sse2<-NULL

for(L in 1:7)
{
  W=solve((t(X[1:500,])%*%X[1:500,])+(L*diag(500)))%*%(t(X[1:500,])%*%Y[1:500])
  Y_pred=X[501:1000,]%*%W
  sse2[L]=sum((Y_pred-Y[501:1000])^2)
}
min(sse2)
match(min(sse2),sse2)

#LAMBDA=7; SSE2=30788.08
