#setwd("~/Documents/R/Data/02-wheat-wr")
#rm(list=ls())
setwd("~/jorge_localraid/02-Wheat/toNatureWr")

load(file="frwrF1F2.RData"); ls(); dim(frwrF1F2)
freqs<-frwrF1F2[,8:ncol(frwrF1F2)]
set<-list(); k<-1
for(s in 1:4){
  set[[s]]<-c(k:(k+999))
  k<-k+1000
  }
set[[5]]<-c(k:ncol(freqs))

library(doParallel)
cl<-makeCluster(length(set)+3)
registerDoParallel(cl)
getDoParWorkers()

### step 1: i_i, i=1,2,...,5
for(i in 1:length(set)) {
  x1<-as.matrix(freqs[,set[[i]]])
  n1=ncol(x1)
  res<-foreach(f=1:(n1-2), .combine = "c", .inorder = T) %dopar% {
      mrd2<-c()
      mrd <-colMeans((x1[,f]-x1[,(f+1):n1])^2,na.rm=T)
      mrd2<-c(mrd2,mrd)
      return(mrd2)
    }
    mrd <-mean((x1[,(n1-1)]-x1[,n1])^2,na.rm=T)
    mrd2<-c(res,mrd)
    save(mrd2,file=paste("mrd2Vec_",i,"_",i,".RData",sep=""))
}

### step 2: i_j, i diff j, i=1,2,...,4; j=(i+1),...,5

for(i in 1:(length(set)-1)) {
    x1<-as.matrix(freqs[,set[[i]]])
    n1<-ncol(x1)
  for(j in  (i+1):length(set)) {
    x2<-as.matrix(freqs[,set[[j]]])
    n2=ncol(x2)
    res<-foreach(f=1:n1, .combine = "c", .inorder = T) %dopar% {
      mrd2<-c()
      mrd <-colMeans((x1[,f]-x2[,1:n2])^2,na.rm=T)
      mrd2<-c(mrd2,mrd)
      return(mrd2)
    }
    mrd2<-res
    save(mrd2,file=paste("mrd2Vec_",i,"_",j,".RData",sep=""))
  }
}
stopCluster(cl)


