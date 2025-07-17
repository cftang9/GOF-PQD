rm(list=ls(all=TRUE))
source("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/PQD_GOF.R")
Data = read.csv("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data/3_Necrotizing_enterocolities_data/NEC.csv")

n = length(Data$Birth.weight)

#pdf("BirthWeight(log)_vs_TimetoNEC(log).pdf", width=12, height=7)
par(mfrow=c(1,2))
par(mar=c(4,4,3,0.2))
plot(log(Data$Birth.weight),log(Data$Time.to.NEC), 
     xlab="",ylab="")
mtext(paste("Birth", "weight"), side=1, line=2.45,cex=1.35)
mtext(paste("Time to NEC", "diagnosis"), side=2, line=2.45,cex=1.35)
plot(rank(Data$Birth.weight)/(n+1),rank(Data$Time.to.NEC)/(n+1), 
     xlab="", ylab="")
mtext(paste("Birth", "weight"), side=1, line=2.45,cex=1.35)
mtext(paste("Time to NEC", "diagnosis"), side=2, line=2.45,cex=1.35)
par(mfrow=c(1,1))
#dev.off()

Data = cbind(Data$Birth.weight,Data$Time.to.NEC)
Result = PQD_GOF(Data,method="ALL",Figure=T,log_Fig=T)
