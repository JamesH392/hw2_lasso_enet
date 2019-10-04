

# n=20; p=60; k=4
# x=matrix(rnorm(n*p),n,p)
# b= rt(k,20)*5 ;
# y=x[,1:k] %*% b+rnorm(n)*2 
# par(mfrow=c(1,3))
# test<-myglmnet(x,y,a=0.5)
# test$lst


library(glmnet) ;
set.seed(2019);
# require(roxygen2)
# library(devtools) ;
# load_all()

myglmnet = function(z, y, a=0.5) { #ridge(a=0) lasso (a=1) enet(a=any);
  # Center and scale variables
  z = as.matrix(t((t(z)-apply(z,2,mean))/apply(z,2,sd))) # Find lambda by CV plot(u<-cv.glmnet(x,y,alpha=a))
  # Plot full solution path
  plot(glmnet(z,y,alpha=a )); #L1norm
  plot(u<-cv.glmnet(x,y,nfolds=4,alpha=a)) #log(lam)
  lam=c(u$lambda.1se,u$lambda.min)
  v <- glmnet(z,y,alpha=a,lambda=lam)
  # Plot Lambda path
  plot(v)
  # Output lambda and estimates
  lst<-list(lambda=lam,beta=v$beta)
}

