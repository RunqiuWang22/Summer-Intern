memory.limit(size=56000)
newAE=read.csv(file="C:/Users/rwang102/Downloads/CVM/data/ACS3001/final_data.csv",header=T)
attach(newAE)

#------deal with missing value------
a=rep(0,dim(newAE)[2])
for (i in 1:dim(newAE)[2]){
  a[i]=sum(is.na(newAE[,i]))
}
names(a)=names(newAE)
a
newAE$HEIGHT[which(is.na(newAE$HEIGHT)=='TRUE')]=median(newAE$HEIGHT,na.rm=T)
newAE$WEIGHT[which(is.na(newAE$WEIGHT)=='TRUE')]=median(newAE$WEIGHT,na.rm=T)
newAE$BMI[which(is.na(newAE$BMI)=='TRUE')]=median(newAE$BMI,na.rm=T)
newAE$AGE[newAE$AGE<=65]=1
newAE$AGE[newAE$AGE>65&newAE$AGE<=75]=2
newAE$AGE[newAE$AGE>75]=3
newAE$AGE=as.factor(newAE$AGE)
newAE$bleeding_outcome=as.factor(newAE$bleeding_outcome)
newAE$BMI=cut(newAE$BMI,breaks=c(0,18.5,25,Inf))

xfactors=model.matrix(bleeding_outcome ~AGE+BMI+ETHNIC+RACE+SEX+ALCOHOL+SMOKE+DIABET+BLHRTFFL+BLHYPFL+BLMIFL,data=newAE)[, -1]
x1=data.frame(newAE$HEIGHT,newAE$WEIGHT,xfactors)
x=as.matrix(x1)
y=as.factor(bleeding_outcome)
data=data.frame(x,y)
colnames(data)=c(colnames(x1),"bleeding_outcome")
set.seed(333)
ntest=sample (1:nrow (data),round(nrow (data)/4),replace =F)
train=data[-ntest,]
test=data[ntest,]
library(ROSE)
train=ovun.sample(bleeding_outcome~.,data=train,method ="both")$data