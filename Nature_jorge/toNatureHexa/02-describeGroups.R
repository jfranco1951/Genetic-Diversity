#setwd("~/jorge_localraid/02-Wheat/toNatureHexa/")
#rm(list=ls())
load(file="mrdMatLast.RData"); ls(); dim(mrdmlast)
x2<-read.csv("mrdgr2-50.csv"); head(x2)
selgr<-data.frame(x2[,7],c(1:54636))          ## be careful
colnames(selgr)<-c("group","col"); head(selgr)
ngr<-length(unique(selgr$group))
desc<-data.frame(matrix(NA,(ngr+1),4))
colnames(desc)<-c("group","Ni","mrdMean","mrdStdev")  ; k=0
for(i in 1:ngr){k<-k+1
  sel<-selgr[selgr$group==i,2]
  mat<-as.dist(mrdmlast[sel,sel])
  desc[k,1]<-i;desc[k,2]<-length(sel)
  desc[k,3]<-mean(mat,na.rm=T);desc[k,4]<-sqrt(var(mat,na.rm=T))
}
mat<-as.dist(mrdmlast); k<-k+1
desc[k,1]<-0;desc[k,2]<-nrow(mrdmlast)
desc[k,3]<-mean(mat,na.rm=T);desc[k,4]<-sqrt(var(mat,na.rm=T))
write.csv(desc,"groupsDesc.csv")

