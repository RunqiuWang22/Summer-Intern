fit=glm(bleeding_outcome~.,data=train,family = binomial())
#fit=step(fit,direction = 'both')
pre=ifelse(predict(fit,test)>0.5,1,0)
table=table(test[,"bleeding_outcome"],pre)
accuracy=sum(diag(table))/sum(table)
accuracy



library(ROCR)
pre=predict(fit,test)
pred=prediction(pre,test[,"bleeding_outcome"])
perf=performance(pred,"tpr","fpr")
plot (perf,colorize = TRUE )
grid (21,21,lwd = 1)
points(c(0,1),c(0 ,1),type ="l",lty =2,lwd =2,col ="grey")
auc.tmp=performance (pred ,"auc")
auc=as.numeric(auc.tmp@y.values )
auc=round (auc,4)
auc