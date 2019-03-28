library(gbm)
library(MASS)#package with the boston housing dataset

Boston.boost=gbm(bleeding_outcome~.,data=train,distribution = "gaussian",n.trees = 10000,shrinkage = 0.01, interaction.depth = 4)
pre.Boost=ifelse(predict(Boston.boost,test,n.trees = 500)>1.5,1,0)
table=table(test[,"bleeding_outcome"],pre.Boost)
accuracy=sum(diag(table))/sum(table)
accuracy
summary(Boston.boost,las=2)

library(ROCR)
pred=prediction((pre.Boost-1.2),test[,"bleeding_outcome"])
perf=performance(pred,"tpr","fpr")
plot (perf,colorize = TRUE )
grid (21,21,lwd = 1)
points(c(0,1),c(0 ,1),type ="l",lty =2,lwd =2,col ="grey")
auc.tmp=performance (pred ,"auc")
auc=as.numeric(auc.tmp@y.values )
auc=round (auc,4)
auc