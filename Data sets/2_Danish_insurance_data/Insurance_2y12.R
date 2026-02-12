rm(list=ls(all=TRUE))
source("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/PQD_GOF.R")
# the losses (all positive) of last two years (1989-1990) 
Data = read.csv("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/2_Danish_insurance_data/Danish_2y_all_pos.csv")

Data = Data[,c(2,3)]; 

n = length(Data[,1])

#pdf("Danish_y2_12.pdf", width=12, height=7)
par(mfrow=c(1,2))
par(mar=c(4,4,3,0.1))
plot(log(Data[,1]),log(Data[,2]),xlab="",ylab="")
mtext("Buildings", side=1, line=2.45,cex=1.35)
mtext("Contents", side=2, line=2.45,cex=1.35)
plot(rank(Data[,1])/(n+1), rank(Data[,2])/(n+1),xlab="",ylab="")
mtext("Buildings", side=1, line=2.45,cex=1.35)
mtext("Contents", side=2, line=2.45,cex=1.35)
par(mfrow=c(1,1))
#dev.off()

Results = PQD_GOF(Data,Figure=T,log_Fig=T)
