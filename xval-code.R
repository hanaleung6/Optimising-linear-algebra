
## making a design matrix with all the interactions

## The easy way: get lm to do it
## assuming sgemm1 is your subset of the data frame with the predictors and logged Run 1 times

model <- lm(logrun1~.^2,data=sgemm1)
X<-model.matrix(model)[,-1]  ## drop the intercept column because regsubsets() will put it back in

## Without actually running lm()
## faster, and still works when there are more variables than observations

mf<-model.frame(logrun1~.^2, data=sgemm1)
X<-model.matrix(logrun1~.^2, mf)[,-1]

## crossvalidation code

allyhat<-function(xtrain, ytrain, xtest,lambdas,nvmax=50){
  n<-nrow(xtrain)
  yhat<-matrix(nrow=nrow(xtest),ncol=length(lambdas))
  search<-regsubsets(xtrain,ytrain, nvmax=nvmax, method="back")
  summ<-summary(search)
  for(i in 1:length(lambdas)){
    penMSE<- n*log(summ$rss)+lambdas[i]*(1:nvmax)
    best<-which.min(penMSE)  #lowest penMSE
    betahat<-coef(search, best) #coefficients
    xinmodel<-cbind(1,xtest)[,summ$which[best,]] #predictors in that model
    yhat[,i]<-xinmodel%*%betahat
  }
  yhat
}

n<-nrow(X)
folds<-sample(rep(1:10,length.out=n))
lambdas<-c(2,4,6,8,10,12)
fitted<-matrix(nrow=n,ncol=length(lambdas))
for(k in 1:10){
  train<- (1:n)[folds!=k]
  test<-(1:n)[folds==k]
  fitted[test,]<-allyhat(X[train,],y[train],X[test,],lambdas)  
}
colMeans((y-fitted)^2)
