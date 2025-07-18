rm(list=ls(all=TRUE))
library(xtable)

load("Sim_CA_N005.Rdata")
C1 = Test_b
load("Sim_CA_N010.Rdata")
C2 = Test_b
load("Sim_CA_N015.Rdata")
C3 = Test_b
load("Sim_CA_N020.Rdata")
C4 = Test_b
load("Sim_CA_N025.Rdata")
C5 = Test_b

B = 1000
temp_power = array(0,5)
temp_time = array(0)
for(k in 1:5){
  for(b in 1:B){
    temp_power[k] = get(paste0("C",k))[[b]]$Rejection/B + temp_power[k]
    temp_time = get(paste0("C",k))[[b]]$Diff.Time/B + temp_time
  }
}

temp_power
temp_time/5
