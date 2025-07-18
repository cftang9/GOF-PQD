rm(list=ls(all=TRUE))
library(xtable)

load("Sim_CA_N005.rdata")
T02 = Test_b
load("Sim_CA_N010.rdata")
T04 = Test_b
load("Sim_CA_N015.rdata")
T06 = Test_b
load("Sim_CA_N020.rdata")
T08 = Test_b
load("Sim_CA_N025.rdata")
T10 = Test_b

B = 1000

power_CA = array(0,5); 
Time_CA = 0; 
for(b in 1:B){
  power_CA[1] = T02[[b]]$Rejection/B + power_CA[1]
  power_CA[2] = T04[[b]]$Rejection/B + power_CA[2]
  power_CA[3] = T06[[b]]$Rejection/B + power_CA[3]
  power_CA[4] = T08[[b]]$Rejection/B + power_CA[4]
  power_CA[5] = T10[[b]]$Rejection/B + power_CA[5]
  Time_CA = (Time_CA 
              + (T02[[b]]$Diff.Time 
                 + T04[[b]]$Diff.Time
                 + T06[[b]]$Diff.Time
                 + T08[[b]]$Diff.Time
                 + T10[[b]]$Diff.Time)
  )
}

power_CA
Time_CA/(B*5)
