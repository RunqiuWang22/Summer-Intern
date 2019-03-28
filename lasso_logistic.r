library(glmnet)
set.seed(111)
fit1=cv.glmnet(x[-ntest ,],y[-ntest],alpha=1,family="binomial") ##train data
fit2=glmnet(x[ntest ,],y[ntest],alpha=1,lambda=fit1$lambda.min,family="binomial")
t=as.matrix(coef(fit2,s=fit2$lambda))
plot(fit1$glmnet.fit, "lambda", label=TRUE,main="Lasso")


logit.pre= ifelse (predict(fit2,x[ntest,],type = 'response')>0.5,1,0)
table = table (y[ntest] , logit.pre)
accuracy_lasso=sum(diag(table))/sum(table)
accuracy_lasso


library(ROCR)
pred=predict(fit2,x[ntest,],type='response')[,1]
pred=prediction(pred,y[ntest])
perf=performance(pred,"tpr","fpr")
plot (perf,colorize = TRUE )
grid (21,21,lwd = 1)
points(c(0,1),c(0 ,1),type ="l",lty =2,lwd =2,col ="grey")
auc.tmp=performance (pred ,"auc")
auc=as.numeric(auc.tmp@y.values )
auc_lasso=round (auc,4)
auc_lasso

write.csv(t,'C:/Users/rwang102/Downloads/CVM/data/t.csv')

a=c(names(data2),rep(0,dim(data1)[2]-dim(data2)[2]))



