setwd("~/jorge_localraid/02-Wheat/toNatureHexa/")
require(plyr)
load(file="idHexa.RData"); ls(); dim(idHexa)
na=nrow(idHexa); nm=na/2 
## Pop 
  load(file="frHexaLast.RData"); dim(frHexaLast)
  fr<-frHexaLast
  gnames=colnames(fr)
  ng=length(gnames)
  nefgen<- apply(fr,1,function(y) sum(!is.na(y))) 
  pest  <- apply(fr,1,mean,na.rm=T)
  pest2 <- pest^2
  pmiss <- apply(fr,1,function(y) sum(is.na(y))/length(y))
  nhom  <- apply(fr,1,function(y) sum(y==1 | y==0, na.rm=T))
  lpest <- log2(pest); summary(lpest)  ### non informative log(0)=-inf
  shan  <- -(pest*lpest); summary(shan)
  ho    <- 1-nhom/nefgen; summary(ho)
  pm    <- data.frame(idHexa[,1:5],pmiss,nefgen,pest,pest2,nhom,ho,shan)
### he, ae, ho, shannon "by" set,mark
  he<-ddply(pm[,c(1,4,9)], .(set,mark), function(y) {1-sum(y[,3],na.rm=T)})[,3]
  he2<-2*he
  ae<-1/(1-he)
  shannon<-ddply(pm[,c(1,4,12)],.(set,mark), function(y) sum(y[,3], na.rm=T))[,3]
  ind<-data.frame(pm[pm$alle==1,-c(9,12)],he2,ae,shannon)
  ind$inb<-ind$he-ind$ho
##  colnames(ind)=c("set","AlleleID","CloneID","mark","alle","pmiss",
#                    "nefgen","pest","nhom","ho","he2","ae","shannon","inb")
  save(ind,file="indPopHexa.RData")

  ## Core
  x<-read.csv("coreHexaLast.csv",head=T); cols<-as.numeric(x[,1])
  fr<-frHexaLast[,cols]
  gnames=colnames(fr)
  ng=length(gnames)
  nefgen<- apply(fr,1,function(y) sum(!is.na(y))) 
  pest  <- apply(fr,1,mean,na.rm=T)
  pest2 <- pest^2
  pmiss <- apply(fr,1,function(y) sum(is.na(y))/length(y))
  nhom  <- apply(fr,1,function(y) sum(y==1 | y==0, na.rm=T))
  lpest <- log2(pest); summary(lpest)  ### non informative log(0)=-inf
  shan  <- -(pest*lpest); summary(shan)
  ho    <- 1-nhom/nefgen; summary(ho)
  pm    <- data.frame(idHexa[,1:5],pmiss,nefgen,pest,pest2,nhom,ho,shan)
  ## he, ae, ho, shannon "by" set,mark
  he<-ddply(pm[,c(1,4,9)], .(set,mark), function(y) {1-sum(y[,3],na.rm=T)})[,3]
  he2<-2*he
  ae<-1/(1-he)
  shannon<-ddply(pm[,c(1,4,12)],.(set,mark), function(y) sum(y[,3], na.rm=T))[,3]
  ind<-data.frame(pm[pm$alle==1,-c(9,12)],he2,ae,shannon)
  ind$inb<-ind$he-ind$ho
  #  colnames(ind)=c("set","AlleleID","CloneID","mark","alle","pmiss",
  #                    "nefgen","pest","nhom","ho","he2","ae","shannon","inb")
  save(ind,file="indCoreHexa.RData")
  
