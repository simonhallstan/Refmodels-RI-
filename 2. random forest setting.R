#A simple exercise for setting values for nodesize, sampsize and mtry in randomForest modelling
#Used for

require(randomForest)
library(PresenceAbsence) #For AUC calculation



#loops for nodesize, sampsize and mtry

#nodesize
###########################################
nodesize.results<-matrix(data = NA, nrow = 50, ncol = ncol(taxa.ref))
colnames(nodesize.results)<-colnames(taxa.ref)
rownames(nodesize.results)<-1:50
nodesize.results.auc<-nodesize.results


for(taxon in colnames(taxa.ref))
{
for(nodesize in 1:50) 
 {
  data.tmp<-data.frame(taxa.ref[,taxon], env.ref)
  model.RF<-randomForest(as.factor(data.tmp[,1]) ~ .,data=data.tmp[,-1], importance=F, nodesize=nodesize, ntree=1000)
  nodesize.results[nodesize,taxon]<-model.RF$err.rate[1000,'OOB']
  
  modelresults.tmp<-data.frame(rownames(taxa.ref),taxa.ref[,taxon],model.RF$votes[,'1'])
  nodesize.results.auc[nodesize,taxon]<-auc(modelresults.tmp, na.rm=T)[1,'AUC']
  print(nodesize)
}
  print(taxon)
}

#sampsize
###########################################
sampsize.results<-matrix(data = NA, nrow = 41, ncol = ncol(taxa.ref))
colnames(sampsize.results)<-colnames(taxa.ref)
rownames(sampsize.results)<-10:50
sampsize.results.auc<-sampsize.results

for(taxon in colnames(taxa.ref))
{
  for(sampsize in 10:50) 
  {
    data.tmp<-data.frame(taxa.ref[,taxon], env.ref)
    model.RF<-randomForest(as.factor(data.tmp[,1]) ~ .,data=data.tmp[,-1], importance=F, sampsize=sampsize, ntree=1000)
    sampsize.results[sampsize-9,taxon]<-model.RF$err.rate[1000,'OOB']
    modelresults.tmp<-data.frame(rownames(taxa.ref),taxa.ref[,taxon],model.RF$votes[,'1'])
    sampsize.results.auc[sampsize-9,taxon]<-auc(modelresults.tmp, na.rm=T)[1,'AUC']
    print(sampsize)
  }
  print(taxon)
}


#mtry
###########################################
mtry.results<-matrix(data = NA, nrow = 11, ncol = ncol(taxa.ref))
colnames(mtry.results)<-colnames(taxa.ref)
rownames(mtry.results)<-1:11
mtry.results.auc<-mtry.results

for(taxon in colnames(taxa.ref))
{
  for(mtry in 1:11) 
  {
    data.tmp<-data.frame(taxa.ref[,taxon], env.ref)
    model.RF<-randomForest(as.factor(data.tmp[,1]) ~ .,data=data.tmp[,-1], importance=F, mtry=mtry, ntree=1000)
    mtry.results[mtry,taxon]<-model.RF$err.rate[1000,'OOB']
    modelresults.tmp<-data.frame(rownames(taxa.ref),taxa.ref[,taxon],model.RF$votes[,'1'])
    mtry.results.auc[mtry,taxon]<-auc(modelresults.tmp, na.rm=T)[1,'AUC']
    print(mtry)
  }
  print(taxon)
}

#default settings for comparison
###########################################
default.results<-matrix(data = NA, ncol = 1, nrow = ncol(taxa.ref))
rownames(default.results)<-colnames(taxa.ref)
colnames(default.results)<-c('AUC')


for(taxon in colnames(taxa.ref))
{
    data.tmp<-data.frame(taxa.ref[,taxon], env.ref)
    model.RF<-randomForest(as.factor(data.tmp[,1]) ~ .,data=data.tmp[,-1], importance=F, ntree=1000)
    default.results[taxon,'AUC']<-auc(modelresults.tmp, na.rm=T)[1,'AUC']
    print(taxon)
}

mean(default.results[,'AUC'])

#############
#save results or find the lowest mean (across taxon)

write.table(nodesize.results, "nodesize.results.txt", sep="\t", dec=",", col.names=NA)
write.table(sampsize.results, "sampsize.results.txt", sep="\t", dec=",", col.names=NA)
write.table(mtry.results, "mtry.results.txt", sep="\t", dec=",", col.names=NA)

write.table(nodesize.results.auc, "nodesize.results.auc.txt", sep="\t", dec=",", col.names=NA)
write.table(sampsize.results.auc, "sampsize.results.auc.txt", sep="\t", dec=",", col.names=NA)
write.table(mtry.results.auc, "mtry.results.auc.txt", sep="\t", dec=",", col.names=NA)

sort(apply(nodesize.results.auc,1, mean) )
sort(apply(sampsize.results.auc,1, mean) )
sort(apply(mtry.results.auc,1, mean) )

which(apply(nodesize.results.auc,1, mean)==max(apply(nodesize.results.auc,1, mean)))
which(apply(sampsize.results.auc,1, mean)==max(apply(sampsize.results.auc,1, mean)))
which(apply(mtry.results.auc,1, mean)==max(apply(mtry.results.auc,1, mean)))
     
     
