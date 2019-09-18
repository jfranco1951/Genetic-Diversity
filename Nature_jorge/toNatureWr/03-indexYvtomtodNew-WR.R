setwd("~/jorge_localraid/02-Wheat/toNatureWr/")
#rm(list=ls())

set<-list(); k<-1
for(s in 1:4){
  set[[s]]<-c(k:(k+999))
  k<-k+1000
}
set[[5]]<-c(k:4403)
library(doParallel)
ncores=20
cl<-makeCluster(length(set)+1)
registerDoParallel(cl)
getDoParWorkers()

### create index i,i
for(i in 1:length(set)) {
  n1=length(set[[i]])
  res<-foreach(f=1:(n1-1), .combine = "c", .inorder = T) %dopar% {
    index<-c()
    ind<-rep(set[[i]][f],(n1-f))
    index<-c(index,ind)
    return(index)
  }
  index<-res
  save(index,file=paste("index_",i,"_",i,".RData",sep=""))
}
##create index i,j

for(i in 1:(length(set)-1)) {
  n1=length(set[[i]])
  for(j in (i+1):length(set)) {
    n2=length(set[[j]])
    res<-foreach(f=1:n1, .combine = "c", .inorder = T) %dopar% {
      index<-c()
      ind<-rep(set[[i]][f],n2)
      index<-c(index,ind)
      return(index)
    }
    index<-res
    save(index,file=paste("index_",i,"_",j,".RData",sep=""))
  }
}

### join the indices over sets
res<-foreach(i = 1:length(set), .combine="c", .inorder=T) %dopar% { 
  ind1<-c()
  for(j in i:length(set)){
    load(file=paste("index_",i,"_",j,".RData", sep=""))
    ind1<-c(ind1,index)  
  }
  return(ind1)
}
index<-res
length(index)
save(index,file="indexNew.RData")

load(file="mrdVec.RData"); ls(); length(mrdVec)
mrdset<-split(mrdVec,index)
save(mrdset, file="mrdsetNew.RData")

### create mrdTri
load(file="mrdsetNew.RData")
for(s in 1:length(set)) {aux<-mrdset[set[[s]][1]:set[[s]][length(set[[s]])]]
n1<-(s-1)*1000
system.time(
  mrdTri<-foreach(i = 1:length(aux),.combine="cbind",.inorder=T) %dopar% {
    mrm<-c(rep(0,(n1+i)),unlist(aux[[i]]))
    return(mrm)
  })
save(mrdTri,file=paste("mrdTri",s,".RData", sep=""))
print(dim(mrdTri))
}

load(file="mrdTri1.RData"); mrdT<-mrdTri
for(i in 2:length(set)){
  load(file=paste("mrdTri",i,".RData",sep=""))
  mrdT<-cbind(mrdT,mrdTri)
}

#mrdT<-cbind(mrdT,rep(0,13));
dim(mrdT)
save(mrdT, file="mrdTriNew.RData")
mrdm<-t(mrdT)+mrdT
dim(mrdm)

## read names of original file and save matrices
x<-read.csv(file="colnamesWR.csv")    ### they are X-names
names<-as.character(x[,2])
colnames(mrdm)<-names
rownames(mrdm)<-names
save(mrdm,file="mrdMatNew.RData")
mrdd<-as.dist(mrdm)
save(mrdd,file="mrdDistNew.RData")

stopCluster(cl)
