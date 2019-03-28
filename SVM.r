library(e1071)
SVM.model=svm(x[-ntest ,],y[-ntest])
pre.SVM=predict(SVM.model,x[ntest,],type='response')
table=table(y[ntest],pre.SVM)
accuracy=sum(diag(table))/sum(table)
accuracy
library(caret)
varImp(SVM.model, scale = FALSE)