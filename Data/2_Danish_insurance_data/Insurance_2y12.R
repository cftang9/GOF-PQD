rm(list=ls(all=TRUE))
source("PQD_GOF_ALL.r")
library(fitdistrplus)
data(danishmulti)

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

PQD_GOF_ALL(Data,Figure=F)

# $EL_A
# # $TS
# [1] 1.483756
# 
# $CV
# [1] 1.307858
# 
# $pvalue
# [1] 0.0331
# 
# $Time
# Time difference of 0.06535697 secs
# 
# $EL_F
# $TS
# [1] 1.483756
# 
# $CV
# 95% 
# 1.145917 
# 
# $pvalue
# [1] 0.0219
# 
# $Time
# Time difference of 15.4529 secs
# 
# 
# $DS_04
# $TS
# [1] 9.529961
# 
# $CV
# [1] 10.776
# 
# $pvalue
# [,1]
# [1,] 0.07851602
# 
# $Time
# Time difference of 0.766382 secs
# 
# 
# $S_05
# $TS
# [1] 0.6677139
# 
# $CV
# [1] 0.6589889
# 
# $pvalue
# [1] 0.0457
# 
# $Time
# Time difference of 41.14559 secs
# 
# 
# $GS_10
# # $TS
# [1] 3.550139
# 
# $CV
# [1] 12.15873
# 
# $pvalue
# [1] 0.2971
# 
# $Time
# Time difference of 167.7704 secs

# 
# 
# $GS_13
# $TS
# [1] 1.183062
# 
# $CV
# [1] 0.7794473
# 
# $pvalue
# [1] 0.015
# 
# $Time
# Time difference of 4125.69 secs
# 
# 
# $LW_14
# $TS
# [1] -3.531271
# 
# $CV
# [1] -3.033578
# 
# $pvalue
# [1] 0.01754
# 
# $Time
# Time difference of 59.82212 secs
# 
# LG_22
# $TS
# [1] 1.139104
# 
# $CV
# [1] 1.293443
# 
# $pvalue
# [1] 0.07
# 
# $Time
# Time difference of 30963.5 secs
