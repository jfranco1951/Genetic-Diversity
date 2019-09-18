#setwd("~/jorge_localraid/coresWr/toNatureHexa/")
#rm(list=ls())
require(fastcluster)
load(file="mrdDistLast.RData"); ls(); length(mrdd)
object.size(mrdd)
### cluster analysis
hc<-hclust(mrdd, method="ward.D2")  ## mrd is not a squared distance 
save(hc,file="hcMRD.RData")

# load(file="hcMRD.RData"); ls()    ## if starting here
gro<-as.data.frame(hc$labels); k=0
#for(i in c(2:15,seq(20,50,5))) { 
for(i in 2:50) { # k<-k+1
  pp=cutree(hc,k=i)
  gro<-data.frame(gro,pp)
}
colnames(gro)<-c("geno",paste("ngr",c(2:50),sep=""))
write.csv(gro,"mrdgr2-50.csv")
### validation
gro<-read.cv("mrdgr2-50.csv"); head(gro)   # if starting here
#load(file="mrdDistLast.RData")
require(fpc)
vale<-list(); k=1
for(i in 2:50) { k<-k+1
vale[[k]] <-cluster.stats(d = mrdd, gro[,k],silhouette=F, wgap=F, sepindex=F,
                          sepwithnoise=F)
}
length(vale)
#str(vale[[6]])
for(i in 1:length(vale)) {
  print(c(vale[[i]]$cluster.number,vale[[i]]$average.between,vale[[i]]$average.within,
          vale[[i]]$within.cluster.ss,vale[[i]]$ch) )
}

