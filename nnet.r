library(nnet)
set.seed(334)
model=nnet(x[-ntest,],class.ind(y[-ntest]),maxit=2000,decay=5e-6, rang = 0.1,size=10,trace=F,linout=T)
pre.network=ifelse(predict(model,x[ntest,],type='raw')[,1]>predict(model,x[ntest,],type='raw')[,2],0,1)
table=table(y[ntest],pre.network)
accuracy_nnet=sum(diag(table))/sum(table)
accuracy_nnet
b=varImp(model)[,2]
b1=sort(b,decreasing = TRUE)
#names(x1)[c(1,4,27,13,26,9,2,8,32,3,28)]
b2=names(x1)[c(1,16,8,19,7,10,9,2,24,17)]


library(ROCR)
pre1=predict(model,x[ntest,],type='raw')[,2]
pred=prediction(pre1,y[ntest])
perf=performance(pred,"tpr","fpr")
plot (perf,colorize = TRUE )
grid (21,21,lwd = 1)
points(c(0,1),c(0 ,1),type ="l",lty =2,lwd =2,col ="grey")
auc.tmp=performance (pred ,"auc")
auc=as.numeric(auc.tmp@y.values )
auc=round (auc,4)
auc

write.csv(b2,'C:/Users/rwang102/Downloads/CVM/data/b.csv')



