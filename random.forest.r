library(randomForest)
forest.model=randomForest (x[-ntest ,],y[-ntest],importance =TRUE)#ntree=200,mtry=20
# predict test set
pre.forest= ifelse (predict(forest.model,test,type = 'prob')[,2]>0.5,1,0)
table=table(y[ntest],pre.forest)
accuracy=sum(diag(table))/sum(table)
accuracy
###
library(ROCR) # plot ROC
pred <- predict(forest.model, test,decision.values=TRUE,type ="class")
pred =as.numeric (as.character(pred))
p <-attr(pred ,"decision . values")
pred <- prediction (pred,y[ntest])
perf=performance (pred,"tpr ","fpr ")
plot (perf , colorize = TRUE )
grid (21 , 21, lwd = 1)
points (c(0 ,1) , c(0 ,1) , type ="l", lty =2, lwd =2, col =" grey ")
auc.tmp <- performance (pred ,"auc")
auc <- as.numeric(auc.tmp@y.values )
auc <- round (auc , 4)
auc
###
#---------------------------Roc curve---------------------------------------------
library(ROCR)
pre.forest1=predict(forest.model,test,type = 'prob')[,2]
pred=prediction(pre.forest1,y[ntest])
perf=performance(pred,"tpr","fpr")
plot (perf,colorize = TRUE )
grid (21,21,lwd = 1)
points(c(0,1),c(0 ,1),type ="l",lty =2,lwd =2,col ="grey")
auc.tmp=performance (pred ,"auc")
auc=as.numeric(auc.tmp@y.values )
auc=round (auc,4)
auc
#----------------importance plot---------------------------------------------------
model.forest.all=randomForest(x,y,importance =TRUE , proximity = TRUE )
importance(model.forest.all)[1:10,]
varImpPlot(model.forest.all,main ="Importance Plot",type=2)
#-------------------------determine the number of tree-----------------------------
plot (forest.model) # determine the number of tree
#--------------------------determine mtry------------------------------------------
rate=rep(0,ncol(x))
for(i in 1:ncol(x))
{
  set.seed (111)
  model.forest=randomForest(x[-ntest,],y[-ntest],importance =TRUE,proximity=TRUE,ntree=200,mtry=i)
  rate[i]= mean(model.forest$err.rate)
}## Calculate the average error rate of the model
plot(rate)