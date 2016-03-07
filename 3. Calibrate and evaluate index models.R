#
#
#
# 

#Packages
require(leaps)
library(randomForest)
#--------------------------------------------------------

# 1 
#Initial run with full RF model to select top 29 predictors for subset.
#Due to the random part of random forest not the exact same as the rf_env_all model

data.rf<-cbind(Index=index.cal[,1], env.all.cal[,])
refmodel.RF<-randomForest(Index~.,data=data.rf, mtry=2, sampsize=34, nodesize=1)
write.table(refmodel.RF$importance, "results_varimp.env.all.txt", sep="\t", dec=",", col.names=NA)




# 2 
# loop with the different predictor sets

predcitorsets<-c('all','sel','wfd','map')

all.aspt.predictions<-matrix(data = NA, nrow = nrow(taxa.all), ncol = 9)
colnames(all.aspt.predictions)<-c('rf.all', 'rf.sel', 'rf.wfd', 'rf.map', 'lm.sel', 'lm.wfd', 'lm.map', 'types', 'null')
rownames(all.aspt.predictions)<-rownames(taxa.all)
all.mila.predictions<-all.aspt.predictions


for(pset in predcitorsets)
{

ENV<-get(paste('env.',pset, sep=''))
ENV.cal<-get(paste('env.',pset,'.cal', sep=''))
if(pset!='all') ENV.log<-get(paste('env.',pset,'.log', sep=''))
if(pset!='all') ENV.cal.log<-get(paste('env.',pset,'.log', '.cal', sep=''))

#Models

#randomForest
data.aspt.rf<-cbind(ASPT=index.cal[,'ASPT'], ENV.cal)
refmodel.aspt.RF<-randomForest(ASPT~.,data=data.aspt.rf, mtry=2, sampsize=34, nodesize=1)

data.mila.rf<-cbind(MILA=index.cal[,'MILA'], ENV.cal)
refmodel.mila.RF<-randomForest(MILA~.,data=data.mila.rf, mtry=2, sampsize=34, nodesize=1)

#random forest predictions
all.aspt.predictions[,paste('rf.',pset, sep='')]<-predict(refmodel.aspt.RF, newdata=ENV)
all.mila.predictions[,paste('rf.',pset, sep='')]<-predict(refmodel.mila.RF, newdata=ENV)

#lm
if(pset!='all')
{
fullmodel.leap.aspt<-leaps(ENV.cal.log, index.cal[,'ASPT'], method="adjr2", names=colnames(ENV.cal.log), nbest=1)
bestmod<-fullmodel.leap.aspt$which[which(max(fullmodel.leap.aspt$adjr2)==fullmodel.leap.aspt$adjr2),]
data.bestmod.aspt<-cbind(index.cal[,'ASPT',drop=F], ENV.cal.log[,bestmod])
refmodel.aspt.lm<-lm(ASPT~., data=data.bestmod.aspt)

fullmodel.leap.mila<-leaps(ENV.cal.log, index.cal[,'MILA'], method="adjr2", names=colnames(ENV.cal.log), nbest=1)
bestmod<-fullmodel.leap.mila$which[which(max(fullmodel.leap.mila$adjr2)==fullmodel.leap.mila$adjr2),]
data.bestmod.mila<-cbind(index.cal[,'MILA',drop=F], ENV.cal.log[,bestmod])
refmodel.mila.lm<-lm(MILA~., data=data.bestmod.mila)

#lm predictions
all.aspt.predictions[,paste('lm.',pset, sep='')]<-predict(refmodel.aspt.lm, newdata=ENV.log)
all.mila.predictions[,paste('lm.',pset, sep='')]<-predict(refmodel.mila.lm, newdata=ENV.log)


}}

#null model

all.aspt.predictions[,'null']<-mean(index.cal[,'ASPT'])
all.mila.predictions[,'null']<-mean(index.cal[,'MILA'])

#type model
meanindex.type<-matrix(data = NA, nrow = length(levels(droplevels(types.cal[,'Type']))), ncol = 2)
colnames(meanindex.type)<-c('ASPT','MILA')
rownames(meanindex.type)<-levels(droplevels(types.cal[,'Type']))

for(type in levels(droplevels(types.cal[,'Type'])))
  {
  meanindex.type[type,'ASPT']<-mean(index.cal[types.cal[,'Type']==type,'ASPT'])
  meanindex.type[type,'MILA']<-mean(index.cal[types.cal[,'Type']==type,'MILA'])
    }


for(site in rownames(taxa.all)) 
{
site.type<-as.character(types[site,])
  if(TRUE %in% (site.type==levels(droplevels(types.cal[,'Type']))))
{
  all.aspt.predictions[site,'types']<-meanindex.type[site.type,'ASPT']
  all.mila.predictions[site,'types']<-meanindex.type[site.type,'MILA']
}}



#--------------------------------------------------------
#Save results


write.table(all.aspt.predictions, "results_all.aspt.predictions.txt", sep="\t", dec=",", col.names=NA)
write.table(all.mila.predictions, "results_all.mila.predictions.txt", sep="\t", dec=",", col.names=NA)
