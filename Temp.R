# Source the main function from PQD_GOF.R online
rm(list=ls(all=TRUE))
source("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/PQD_GOF.R")
set.seed(07182025)
library(copula)
Data = rCopula(50,frankCopula(iTau(frankCopula(),-0.2)))
Temp = PQD_GOF(Data,method="ALL",Figure=T,log_Fig=F)
