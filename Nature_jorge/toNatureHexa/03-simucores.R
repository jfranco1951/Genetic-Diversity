# simulated cores 
setwd("~/jorge_localraid/02-Wheat/toNatureHexa/")
rm(list=ls())
load(file="mrdMatLast.RData"); ls(); dim(mrdmlast)
#genetic Core
x0=mrdmlast
xx0<-read.csv("mrdgr2-50.csv")
xx <- xx0[,c(1,7)]; colnames(xx) <- c("jfID","ngr7"); head(xx)
#xx=groups; head(xx)
### sample size
## 20%
ni=c(1981,777,1919,1575,2449,847,1391); sum(ni) 

x=data.frame(c(1:nrow(xx)), xx$jfID, xx$ngr7); ng=length(ni)
colnames(x)<- c("ncol","jfID","groups")
x1=split(x,x$groups)
# define number of simulations
nsim=1000; sj=vector("list",ng);s=matrix(0,sum(ni),nsim)
names(sj)=c(paste("gr",1:ng,"_",sep="")); colnames(s)=c(paste("NSIM-",1:nsim,sep=""))
sim=matrix(NA,nsim,2)
# create the "nsim" simulations 
for(j in 1:nsim){ for(i in 1:ng) {
  sj[[i]]=sample(x1[[i]][,1], size=ni[i])}
  s[,j]=as.matrix(unlist(sj, recursive = TRUE, use.names = TRUE)) }
## calculate average of distances
for(j in 1:nsim) {
  st=x0[s[,j],s[,j]]; sim[j,1]=j; print(j)
  ## mean of the squared matrix niXni
  sim[j,2]=sum(st)/(nrow(st)*(nrow(st)-1))
}
colnames(sim)=c("nsimu","mrd")
write.csv(sim,file="simCorelast.csv")  ## mrd for 1K "candidate cores"

# Selected simulation (xxx) 
simNrow<-which.max(sim[,2])
sbest<-as.numeric(sim[simNrow,1])
best=s[,sbest]
#core average mrd distance 
x2=x0[best,best]
x2[x2==0]=NA
genos=colnames(x2)
best1=cbind(best,genos)
write.csv(best1,file="coreHexaLast.csv")
#write.csv(best1,file="coreNDhexa.csv")
coremean=sum(x2,na.rm=T)/(nrow(st)*(nrow(st)-1));coremin=min(x2,na.rm=T);coremax=max(x2,na.rm=T) 
coremin;coremean;coremax
## put ncols, idJF & group to mrdCore
grCore<-x[best,3]
mrdcore<-data.frame(c(1:nrow(x2)),colnames(x2),grCore,x2)
colnames(mrdcore)<-c("ncols","jfID","groups",colnames(x2))
save(mrdcore,file="mrdCoreHexaLast.RData")
#save(mrdcore,file="mrdCoreNDhexa.RData")
#population average mrd distance 
popmean=sum(x0,na.rm=T)/(nrow(x0)*(nrow(x0)-1));popmin=min(x0[x0>0],na.rm=T)
popmax=max(x0,na.rm=T) 
popmin;popmean;popmax


