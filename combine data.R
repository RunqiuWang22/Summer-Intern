demo=read.csv(file="C:/Users/rwang102/Downloads/CVM/data/raw data/ADPD2.csv",header=T)
bleed=read.csv(file="C:/Users/rwang102/Downloads/CVM/data/raw data/ADCFBL.csv",header=T)
demo1=unique(demo
bleed1=unique(bleed)
total=merge(bleed,demo,by="USUBJID")
new_total=unique(total)
write.csv(total,file="C:/Users/rwang102/Downloads/CVM/data/total.csv")
