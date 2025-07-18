rm(list=ls(all=TRUE))
source("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/PQD_GOF.R")
library(fitdistrplus)
data(danishmulti)

# the losses (all positive) of last two years (1989-1990) 
Data = danishmulti[c(1715:2167),]
Data = Data[
  c(Data$Building>0 
    & Data$Contents>0 
    & Data$Profits>0),]
Data = cbind(Data[,2],Data[,3],Data[,4])

Data = Data[,c(1,2)]; 

n = length(Data[,1])

#pdf("Danish_y2_12.pdf", width=12, height=7)
par(mfrow=c(1,2))
par(mar=c(4,4,3,0.1))
plot(log(Data[,1]),log(Data[,2]),xlab="",ylab="")
mtext("Building", side=1, line=2.45,cex=1.35)
mtext("Contents", side=2, line=2.45,cex=1.35)
plot(rank(Data[,1])/(n+1), rank(Data[,2])/(n+1),xlab="",ylab="")
mtext("Building", side=1, line=2.45,cex=1.35)
mtext("Contents", side=2, line=2.45,cex=1.35)
par(mfrow=c(1,1))
#dev.off()

PQD_GOF(Data,Figure=F)
