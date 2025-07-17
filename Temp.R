# Source the main function from PQD_GOF.R online
rm(list=ls(all=TRUE))
source("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/PQD_GOF.R")
set.seed(10000)
library(copula)
Data = rCopula(50,claytonCopula(iTau(claytonCopula(),-0.6)))
PQD_GOF(Data,Figure=T,log_Fig=F)