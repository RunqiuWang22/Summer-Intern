my_data=newAE
head(my_data)
mydata =my_data[,10:27]
my_matrix = as.data.frame(do.call(cbind, lapply(mydata, function(x) table(1:nrow(mydata), x))))

library(caret)
preproc = preProcess(my_matrix)
my_matrixNorm = as.matrix(predict(preproc, my_matrix))
distances = dist(my_matrixNorm, method = "euclidean")
clusterdrug = hclust(distances, method = "ward.D") 
plot(clusterdrug, cex=0.5, labels = FALSE,cex=0.5,xlab = "", sub = "",cex=1.2)

library(dendextend)
dend <- as.dendrogram(clusterdrug) 
# Color the branches based on the clusters:
dend <- color_branches(dend, k=4) #, groupLabels=iris_species)
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
plot(dend)

clusterGroups = cutree(clusterdrug, k = 2)
my_data= cbind(data.frame(Cluster=clusterGroups), mydata, my_data[,4:9])
head(my_data)
observationsH=c()
for (i in seq(1,2)){
  observationsH=c(observationsH,length(subset(clusterdrug, clusterGroups==i)))
}
observationsH =as.data.frame(list(cluster=c(1:2),Number_of_observations=observationsH))
observationsH

z=do.call(cbind,lapply(1:2, function(i) round(colMeans(subset(my_matrix,clusterGroups==i)),2)))
colnames(z)=paste0('cluster',seq(1,2))
z

Age=z[nrow(z),]
z=z[1:(nrow(z)-1),]
my_result=matrix(0,ncol=4,nrow=ncol(mydata))
for(i in seq(1,4)){
  for(j in seq(1,ncol(mydata))){
    q = names(mydata)[j]
    q = as.vector(as.matrix(unique(mydata[q])))
    my_result[j,i]=names(sort(z[q,i],decreasing = TRUE)[1])
  }}

colnames(my_result)=paste0('Cluster',seq(1,4))
rownames(my_result)=names(mydata)
my_result=rbind(Age,my_result)
my_result <- cbind(Attribute =c("Age","Route","Sex","Outcome Code","Indication preferred term","Adverse event"), my_result)
rownames(my_result) <- NULL
my_result