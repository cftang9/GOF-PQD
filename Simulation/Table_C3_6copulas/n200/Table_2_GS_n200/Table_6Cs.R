rm(list=ls(all=TRUE))
library(xtable)

load("Sim_MixFrank_1.Rdata")
C1 = Test_b
load("Sim_MixFrank_2.Rdata")
C2 = Test_b
load("Sim_Asymmetric_Copula.Rdata")
C3 = Test_b
load("Sim_Copula_A.Rdata")
C4 = Test_b
load("Sim_qrm.Rdata")
C5 = Test_b
load("Sim_qrm.Rdata")
C6 = Test_b

B = 1000
temp_power = array(0,6)
temp_time = array(0,6)
for(k in 1:6){
  for(b in 1:B){
    temp_power[k] = get(paste0("C",k))[[b]]$Rejection/B + temp_power[k]
    temp_time[k] = get(paste0("C",k))[[b]]$Diff.Time/B + temp_time[k]
  }
}

temp_power
temp_time 
