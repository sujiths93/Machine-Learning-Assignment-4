#Question 6

X=scan(file="C:/Users/sujit_000/Desktop/timer-series.csv")
y=data.frame(X[2:999],X[1:998])
Y=X[3:1000]
X=y
Y=as.matrix(Y)
X=as.matrix(X)
W=solve(t(X)%*%X)%*%(t(X)%*%Y)
Y_pred=X%*%W
sse=sum((Y_pred-Y)^2)
VAR=sse/998
#ANSWER
# W1=1.0037;W2=-0.94972
# Variance= 0.00235


