#read all files 


setwd("C:/Users/simonh/OneDrive/SLU/WATERSv2/delprojekt/20160304 Reference models/Refmodels-RI-")

#biology
taxa.all<-read.table("inv.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
index.all<-read.table("index.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")

#Environmnetal variables
env.all         <-read.table("env_all.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
env.sel         <-read.table("env_sel.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
env.sel.log     <-read.table("env_sel_log.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
env.wfd         <-read.table("env_wfd.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
env.wfd.log     <-read.table("env_wfd_log.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
env.map         <-read.table("env_map.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
env.map.log     <-read.table("env_map_log.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")

#status and types
status<-read.table("status.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")
types<-read.table("types.txt", sep="\t", dec=",", header=T, row.names=1, encoding="UTF-8")


#check that the rownames are the same for all three files
summary(rownames(env.all)==rownames(taxa.all))
summary(rownames(env.sel)==rownames(taxa.all))
summary(rownames(env.sel.log)==rownames(taxa.all))
summary(rownames(env.wfd)==rownames(taxa.all))
summary(rownames(env.wfd.log )==rownames(taxa.all))
summary(rownames(env.map)==rownames(taxa.all))
summary(rownames(env.all)==rownames(taxa.all))
summary(rownames(env.map.log)==rownames(taxa.all))
summary(rownames(types)==rownames(taxa.all))


#create subsets with only calibration sites
#convert invertebrate matrix to presence-absence

calib<-rownames(status)[which(status[,'status']=='calib')]
#valid<-rownames(status)[which(status[,'status']=='valid')]
references<-rownames(status)[which(status[,'reference']==1)]


#environmental variables
env.ref             <-env.all[references,]
env.all.cal         <-env.all[calib,]
env.sel.cal         <-env.sel[calib,]
env.sel.log.cal     <-env.sel.log[calib,]
env.wfd.cal         <-env.wfd[calib,]
env.wfd.log.cal     <-env.wfd.log[calib,]
env.map.cal         <-env.map[calib,]
env.map.log.cal     <-env.map.log[calib,]

#env.all.val         <-env.all[valid,]
#env.sel.val         <-env.sel[valid,]
#env.sel.log.val     <-env.sel.log[valid,]
#env.wfd.val         <-
#env.wfd.log.val     <-
#env.map.val         <-
#env.map.log.val     <-  

types.cal<-types[calib,,drop=F]

  
#biology
taxa.pa<-taxa.all;taxa.pa[taxa.all>0]<-1

taxa.ref<-taxa.pa[references,]
taxa.ref<-taxa.ref[,colSums(taxa.ref)>3 & colSums(taxa.ref)<(nrow(taxa.ref)-3)] #remove rare and common taxa

taxa.cal<-taxa.pa[calib,]
taxa.cal<-taxa.cal[,colSums(taxa.cal)>3 & colSums(taxa.cal)<(nrow(taxa.cal)-3)] #remove rare and common taxa

#taxa.val<-taxa.pa[valid,colnames(taxa.cal)] #only include taxa present in the calibration data
#taxa.all.seltaxa<-taxa.all[,colnames(taxa.cal)]

index.cal<-index.all[calib,]
#index.val<-index.all[valid,]



