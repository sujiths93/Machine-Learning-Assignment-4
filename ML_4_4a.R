#QUESTION 4


# To identify the three best predictors 

X<-pred1[1:500,]
Y<-pred1_resp[1:500,]
sse<-NULL
X<-as.matrix(X)
Y<-as.matrix(Y)
for(i in 1:length(pred1))
{
  W=solve(t(X[,i])%*%X[,i])%*%(t(X[,i])%*%Y)
  Y_pred=X[,i]%*%W
  sse[i]<-sum((Y_pred-Y)^2)
}
first_attribute<-match(min(sse),sse)
sse<-NULL
for(i in 1:length(pred1))
{
  if(i==first_attribute)
  {
    sse[i]=Inf
    next
  }
  
  W=solve(t(X[,c(first_attribute,i)])%*%X[,c(first_attribute,i)])%*%(t(X[,c(first_attribute,i)])%*%Y)
  Y_pred=X[,c(first_attribute,i)]%*%W
 
  sse[i]<-sum((Y_pred-Y)^2)
}
second_attribute=match(min(sse),sse)
for(i in 1:length(pred1))
{
  if(i==first_attribute||i==second_attribute)
  {
    sse[i]=Inf
    next
  }
  
  W=solve(t(X[,c(first_attribute,second_attribute,i)])%*%X[,c(first_attribute,second_attribute,i)])%*%(t(X[,c(first_attribute,second_attribute,i)])%*%Y)
  Y_pred=X[,c(first_attribute,second_attribute,i)]%*%W
  
  sse[i]<-sum((Y_pred-Y)^2)
}
third_attribute=match(min(sse),sse)
print(paste(first_attribute,second_attribute,third_attribute,sep = " "))

#THE THREE ATTRUIBUTES ARE: 12,47,16

#4b
X<-pred1
Y<-pred1_resp
X<-as.matrix(X)
Y<-as.matrix(Y)

#Finding W on first half of the data on the three calculated attributes
W=solve(t(X[1:500,c(first_attribute,second_attribute,third_attribute)])%*%X[1:500,c(first_attribute,second_attribute,third_attribute)])%*%(t(X[1:500,c(first_attribute,second_attribute,third_attribute)])%*%Y[1:500,])
#testing on second half of data
Y_pred=X[501:1000,c(first_attribute,second_attribute,third_attribute)]%*%W
#calculating the sum of squared error
sse<-sum((Y_pred-Y[501:1000])^2)

#SSE WITH THREE ATTRIBUTES=5.09996878361268




sse1<-NULL
#Finding W on first half of the data on all attributes.
W=solve(t(X[1:500,])%*%X[1:500,])%*%(t(X[1:500,])%*%Y[1:500,])
#testing on second half of data.
Y_pred=X[501:1000,]%*%W
sse1<-sum((Y_pred-Y[501:1000])^2)
print(paste("SSE WITH THREE ATTRIBUTES", sse,"SSE WITH ALL ATTRIBUTES",sse1,sep="  "))


#SSE with ALL ATTRIBUTES= 5.7215067