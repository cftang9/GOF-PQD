rm(list=ls(all=TRUE))

rm(list=ls(all=TRUE))
source("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/PQD_GOF.R")
Data = read.csv("")




library(readxl)
source("PQD_GOF_ALL.r")

Data = read_excel("NCAA attendance_2022-2023.xlsx")
head(Data)
Data = Data[,c(2,3)]

n = length(Data$Men)

plot(Data$Men,Data$Women)

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

cor(Data$Men, Data$Women)
cor(Data$Men, Data$Women,method = "spearman")
cor(Data$Men, Data$Women,method = "kendall")
Data = cbind(Data$Men,Data$Women)
PQD_GOF_ALL(Data,Figure=F)

# $EL_A
# $EL_A$TS
# [1] 0.003269108
# 
# $EL_A$pvalue
# [1] 1
# 
# $EL_A$Time
# Time difference of 0.0372231 secs
# 
# 
# $EL_F
# $EL_F$TS
# [1] 0.003269108
# 
# $EL_F$pvalue
# [1] 0.9996
# 
# $EL_F$Time
# Time difference of 45.9984 secs
# 
# 
# $DS_04
# $DS_04$TS
# [1] 6.235619
# 
# $DS_04$pvalue
# [,1]
# [1,] 0.08111643
# 
# $DS_04$Time
# Time difference of 1.484584 secs
# 
# 
# $S_05
# $S_05$TS
# [1] 0
# 
# $S_05$pvalue
# [1] 1
# 
# $S_05$Time
# Time difference of 80.55433 secs
# 
# 
# $GS_10
# $GS_10$TS
# 0
# 
# $GS_10$pvalue
# [1] 0.9685
# 
# $GS_10$Time
# Time difference of 304.0649 secs
# 
# 
# $GS_13
# $GS_13$TS
# [1] 0
# 
# $GS_13$pvalue
# [1] 1
# 
# $GS_13$Time
# Time difference of 21527.4 secs
# 
# 
# $LW_14
# $LW_14$TS
# [1] 2.245861
# 
# $LW_14$pvalue
# [1] 1
# 
# $LW_14$Time
# Time difference of 203.3265 secs
# 
# 
# $LG_22
# $TS
# [1] 0
# 
# $CV
# [1] 0.04469728
# 
# $pvalue
# [1] 0.65
# 
# $Time
# Time difference of 15312.14 secs
# 
# > temp
# $ad.test
# [1] 0
# 
# $ad.quantiles
# 2.5%           5% 
#   0.0000000000 0.0000000000 
# 10%          50% 
#   0.0000000000 0.0004888399 
# 90%          95% 
#   0.0247312171 0.0446972768 
# 97.5% 
# 0.0609813541 
# 
# $pvalue
# [1] 0.65
# 
# $Bernstein_WithPQD
# [1] 14 61  1
# 
# $Bernstein_WithoutPQD
# [1] 14 61  0
# 
# $truncate.ave
# [1] 0