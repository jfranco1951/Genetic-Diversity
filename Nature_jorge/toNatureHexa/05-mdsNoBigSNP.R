setwd("~/jorge_localraid/02-Wheat/toNatureHexa/")
rm(list=ls())
require(irlba)
GowerDblcen <- function(x, na.rm = TRUE)
{
  cnt <- colMeans(x, na.rm = na.rm)
  x <- sweep(x, 2L, cnt, check.margin = FALSE)
  cnt <- rowMeans(x, na.rm = na.rm)
  sweep(x, 1L, cnt, check.margin = FALSE)
}
load(file="mrdMatLast.RData")
n<-nrow(mrdmlast)
A<-(-0.5)*mrdmlast
B<-GowerDblcen(A); dim(B)
save(B,file="BmatrixTemp.RData")
sol<-partial_eigen(B, n = 5, symmetric = TRUE)
summary(sol)
solval<-sol$values
solvec<-sol$vectors
testV<-t(solvec)%*%solvec
ycoor<-solvec[,]
save(ycoor,file="coordHexaMRDlast.RData")
testV
### distance using 5 dimensions
library(cluster)
dy<-daisy(solvec[,])
stress<-100*sum((dy-as.dist(mrdmlast[,]))^2)/(n*(n-1)/2)
stress


