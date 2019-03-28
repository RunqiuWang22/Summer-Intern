#This is code for data asc3001
afl3001=read.csv(file='C:/Users/rwang102/Downloads/CVM/data/baseline_demography.csv')
STUDYID=rep("39039039AFL3001",dim(afl3001)[1])
afl3001=data.frame(STUDYID,afl3001)

asc3001_demo=read.csv(file='C:/Users/rwang102/Downloads/CVM/data/ACS3001/ADSL.csv')
data=merge(asc3001_demo,afl3001,by=intersect(names(afl3001),names(asc3001_demo)),all=TRUE)
data1=data[,intersect(names(afl3001),names(asc3001_demo))]
write.csv(data1,'C:/Users/rwang102/Downloads/CVM/data/ACS3001/baseline_demography.csv')
#-------------------------------bleeding-----------------------------------------------------
data=read.csv("C:/Users/rwang102/Downloads/CVM/data/ACS3001/base_adttesf.csv",header = T)
attach(data)
data1=data[PARAM=="BLEED REQUIRING MED. ATTENTION"&TTECAT=='BLEEDING EVENT'& AEPOCH=='ITT-TOTAL',]
bleeding=data.frame(data1$STUDYID,data1$USUBJID,data1$CENSORC)
colnames(bleeding)=c('STUDYID','USUBJID','bleeding_outcome')
#test opposite result
c=as.data.frame(table(bleeding[,2]))
c1=which(c$Freq!=1)
#afl3001
afl3001_bleeding=read.csv(file="C:/Users/rwang102/Downloads/CVM/data/bleeding.csv",header=T)
STUDYID=rep("39039039AFL3001",dim(afl3001_bleeding)[1])
afl3001_bleeding=data.frame(STUDYID,afl3001_bleeding)

bleeding=rbind(bleeding,afl3001_bleeding)
colnames(bleeding)=c('STUDYID','USUBJID','bleeding_outcome')
write.csv(bleeding,'C:/Users/rwang102/Downloads/CVM/data/ACS3001/bleeding.csv')
###-------------------merge------------------------------------------------------------
demo=read.csv(file='C:/Users/rwang102/Downloads/CVM/data/ACS3001/baseline_demography.csv',header=T)
result=read.csv(file='C:/Users/rwang102/Downloads/CVM/data/ACS3001/bleeding.csv',header=T)
newdata=merge(demo,result,by=c('STUDYID','USUBJID'),all=FALSE)
write.csv(newdata,file="C:/Users/rwang102/Downloads/CVM/data/ACS3001/newdata.csv")

total=read.csv(file="C:/Users/rwang102/Downloads/CVM/data/ACS3001/newdata.csv",header=T)
total1=total[total$SAFFL=='Y'& total$TRT01P %in% c("Rivaroxaban","Rivaroxaban 2.5 mg BID","Rivaroxaban 5 mg BID"),]
write.csv(total1,file="C:/Users/rwang102/Downloads/CVM/data/ACS3001/final_data.csv")

AE=read.csv("C:/Users/rwang102/Downloads/CVM/data/trans_newdata.csv",header=T)
attach(AE)
newAE=AE[SAFFL=='Y'& TRT01P=='Rivaroxaban',]
write.csv(newAE,file="C:/Users/rwang102/Downloads/CVM/data/ACS3001/AFL_data.csv")
#----------------------------------------data prepartion---------------------------------------------------
#---------------AFL390039---------------------------------------------
newAE=read.csv(file="C:/Users/rwang102/Downloads/CVM/data/ACS3001/AFL_data.csv",header=T)
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
newAE$SBP[which(is.na(newAE$SBP)=='TRUE')]=median(newAE$SBP,na.rm=T)
newAE$BLEJF[which(is.na(newAE$BLEJF)=='TRUE')]=median(newAE$BLEJF,na.rm=T)
newAE$AGE[newAE$AGE<=65]=1
newAE$AGE[newAE$AGE>65&newAE$AGE<=75]=2
newAE$AGE[newAE$AGE>75]=3
newAE$AGE=as.factor(newAE$AGE)
newAE$bleeding_outcome=as.factor(newAE$bleeding_outcome)
levels(newAE$SMOKE)=c("Unknown","CURRENT","STOPPED GREATER THAN 1 YEAR AGO","STOPPED LESS THAN OR EQUAL TO 1 YEAR AGO")
levels(BLHRTFFL)=c("Unknown","N","Y")
newAE$BMI=cut(newAE$BMI,breaks=c(0,18.5,25,Inf))

xfactors=model.matrix(bleeding_outcome ~ AGE+BMI+ETHNIC+RACE+SEX+ALCOHOL+SMOKE+DIABET+BLAFTYP+BLASAFL
                      +BLHRTFFL+BLHYPFL+BLMIFL+BLPPIFL+BLSTTIEM+BLVKAFL+PBASAFL+PBTHIEFL,data=newAE)[, -1]#delete country

#-------------------------------------------------normalize data------------------------------------------------------
library(caret)
preproc = preProcess(xfactors,method=c("scale"))
xfactors = as.matrix(predict(preproc, xfactors))
x1=data.frame(newAE$HEIGHT,newAE$WEIGHT,newAE$BLEJF,newAE$SBP,xfactors)
x=as.matrix(data.frame(newAE$HEIGHT,newAE$WEIGHT,newAE$BLEJF,newAE$SBP,xfactors))
y=as.factor(bleeding_outcome)
#------------------------------------------------Generate trainning data--------------------------------------------------
set.seed(333)
ntest=sample (1:nrow (x),round(nrow (x)/4),replace =F)


#------------------combine 2 studies-----------------------------------
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

#-------------------------------------------------normalize data------------------------------------------------------
library(ggplot2)
library(caret)
preproc = preProcess(xfactors)
xfactors = as.matrix(predict(preproc, xfactors))
x1=data.frame(newAE$HEIGHT,newAE$WEIGHT,xfactors)
x=as.matrix(x1)
y=as.factor(bleeding_outcome)
data=data.frame(x,y)
colnames(data)=c(colnames(x1),"bleeding_outcome")


#------------------------------------------------find correlation factor--------------------------------
corr=findLinearCombos(data)
corr
#-----------------------------------------------generate trainning data-----------------------------------------------
set.seed(333)
ntest=sample (1:nrow (data),round(nrow (data)/4),replace =F)
train=data[-ntest,]
test=data[ntest,]
#-------------------------------------------------unbalanced data---------------------------------------------------------
library(ROSE)
train=ovun.sample(bleeding_outcome~.,data=train,method ="both")$data



