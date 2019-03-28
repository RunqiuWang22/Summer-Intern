library(caret)
library(rpart)
fit=rpart(bleeding_outcome~.,data=train,method ="class",control=rpart.control(minsplit=45,cp=0.001))
#fit=rpart(bleeding_outcome~.,data=train,method ="class")#,control =rpart.control(minsplit =1,minbucket=1, cp=0)
plot(fit,uniform =TRUE,main =" Classification Tree for bleeding events ")
text (fit,all =TRUE,cex =0.7)
varImp(fit)
#info.gain.rpart(fit) 
result=ifelse (predict(fit,test,type = 'prob')[,2]>0.5,1,0)
table=table(test[,"bleeding_outcome"],result)
accuracy=sum(diag(table))/sum(table)
accuracy
#---------------------------Roc curve---------------------------------------------
library(ROCR)
pre.tree=predict(fit,test,type='prob')[,-1]
pred=prediction(pre.tree,y[ntest])
perf=performance(pred,"tpr","fpr")
plot (perf,colorize = TRUE )
grid (21,21,lwd = 1)
points(c(0,1),c(0 ,1),type ="l",lty =2,lwd =2,col ="grey")
auc.tmp=performance (pred ,"auc")
auc=as.numeric(auc.tmp@y.values )
auc=round (auc,4)
auc

a=varImp(fit)
write.csv(a,'C:/Users/rwang102/Downloads/CVM/data/a.csv')