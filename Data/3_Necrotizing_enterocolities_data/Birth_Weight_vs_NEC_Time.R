rm(list=ls(all=TRUE))
source("PQD_GOF_ALL.r")

Data = read.csv("Caffeine_data_updated_062811_CLEAN.csv")
Data = Data[1:45,]
Data = Data[,c(3,10)]


n = length(Data$Birth.weight)

plot(Data$Birth.weight,Data$Time.to.NEC)

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

cor(Data$Birth.weight, Data$Time.to.NEC)
cor(Data$Birth.weight, Data$Time.to.NEC,method = "spearman")
cor(Data$Birth.weight, Data$Time.to.NEC,method = "kendall")
Data = cbind(Data$Birth.weight,Data$Time.to.NEC)
Result = PQD_GOF_ALL(Data,Figure=F)
