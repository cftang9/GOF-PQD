rm(list=ls(all=TRUE))
library(xtable)

load("Sim_Rt_Deg_1.Rdata")
C1 = Test_b
load("Sim_Rt_Deg_2.Rdata")
C2 = Test_b
load("Sim_Rt_Deg_3.Rdata")
C3 = Test_b
load("Sim_Rt_Deg_4.Rdata")
C4 = Test_b
load("Sim_Rt_Deg_5.Rdata")
C5 = Test_b
load("Sim_Rt_Deg_100.Rdata")
C6 = Test_b

B = 1000
temp_power = array(0,6)
temp_time = array(0)
for(k in 1:6){
  for(b in 1:B){
    temp_power[k] = get(paste0("C",k))[[b]]$Rejection/B + temp_power[k]
    temp_time = get(paste0("C",k))[[b]]$Diff.Time/B + temp_time
  }
}

temp_power
temp_time/6
