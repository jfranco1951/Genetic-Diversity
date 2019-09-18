## create one vector containing distances
setwd("~/jorge_localraid/02-Wheat/toNatureWr/")
library(doParallel)
#ncores=18
cl<-makeCluster(18)
registerDoParallel(cl)
getDoParWorkers()
res<-foreach(i = 1:5, .combine="c", .inorder=T) %dopar% {
  mrd<-c()
  for(j in i:5){
    load(file=paste("mrd2Vec_",i,"_",j,".RData", sep=""))
    mrd<-c(mrd,sqrt(mrd2))
  }
  return(mrd)
}
mrdVec<-res
save(mrdVec,file="mrdVec.RData")

