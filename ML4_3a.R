#Question 3

X<-pred1[1:500,]
Y<-pred1_resp[1:500,]
X1<-pred2[1:500,]
Y1<-pred2_resp[1:500,]

X<-as.matrix(X)
Y<-as.matrix(Y)
X1<-as.matrix(X1)
Y1<-as.matrix(Y1)
#WEIGHTS OF FIRST DATA SET
W<-solve(t(X)%*%X)%*%(t(X)%*%Y)

#WEIGHTS OF SECOND DATASET
W1<-solve(t(X1)%*%X1)%*%(t(X1)%*%Y1)

#3b
Y_pred=X%*%W
Y1_pred=X1%*%W1

unbiased_variance=sum((Y_pred-Y)^2)/(length(Y_pred)-length(W))
unbiased_variance1<-sum((Y1_pred-Y1)^2)/(length(Y1_pred)-length(W1))
#VARIANCE ON FIRST DATA SET 0.01077716

#VARIANCE ON SECOND DATA SET =not defined
#Difficulty is faced here because (n-k)=0 and SSE/0 is undefined.


#Quesiton 3c
X<-pred1[501:1000,]
Y<-pred1_resp[501:1000,]
X1<-pred2[501:1000,]
Y1<-pred2_resp[501:1000,]

X<-as.matrix(X)
Y<-as.matrix(Y)
X1<-as.matrix(X1)
Y1<-as.matrix(Y1)

Y_pred<-X%*%W
Y_err<-sum((Y_pred-Y)^2)
#SSE ON FIRST DATA SET 5.721507



Y1_pred<-X1%*%W1
Y1_err<-sum((Y1_pred-Y1)^2)
#SSE ON SECOND DATA SET 32984664