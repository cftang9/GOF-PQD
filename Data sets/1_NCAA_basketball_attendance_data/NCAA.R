rm(list=ls(all=TRUE))
source("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/PQD_GOF.R")
Data = read.csv("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/1_NCAA_basketball_attendance_data/NCAA.csv")
Data = Data[,c(2,3)]

n = length(Data$Men)

#pdf("NCAA.pdf", width=12, height=7)
par(mfrow=c(1,2))
par(mar=c(4,4,3,0.1))
plot(log(Data$Men),log(Data$Women), 
     xlab="",ylab="")
mtext("Men's attendance", side=1, line=2.45,cex=1.35)
mtext("Women's attendance", side=2, line=2.45,cex=1.35)
plot(rank(Data$Men)/(n+1),rank(Data$Women)/(n+1), 
     xlab="", ylab="")
mtext("Men's attendance", side=1, line=2.45,cex=1.35)
mtext("Women's attendance", side=2, line=2.45,cex=1.35)
par(mfrow=c(1,1))
#dev.off()

Data = cbind(Data$Men,Data$Women)
PQD_GOF(Data,Figure=F,log_Fig=T)
