rm(list=ls(all=TRUE))
library(xtable)

load("Sim_FGM_N02.rdata")
T02 = Test_b
load("Sim_FGM_N04.rdata")
T04 = Test_b
load("Sim_FGM_N06.rdata")
T06 = Test_b
load("Sim_FGM_N08.rdata")
T08 = Test_b
load("Sim_FGM_N10.rdata")
T10 = Test_b

B = 1000

power_FGM = array(0,5); 
Time_FGM = 0; 
for(b in 1:B){
    power_FGM[1] = T02[[b]]$Rejection/B + power_FGM[1]
    power_FGM[2] = T04[[b]]$Rejection/B + power_FGM[2]
    power_FGM[3] = T06[[b]]$Rejection/B + power_FGM[3]
    power_FGM[4] = T08[[b]]$Rejection/B + power_FGM[4]
    power_FGM[5] = T10[[b]]$Rejection/B + power_FGM[5]
  Time_FGM = (Time_FGM 
              + (T02[[b]]$Diff.Time 
              + T04[[b]]$Diff.Time
              + T06[[b]]$Diff.Time
              + T08[[b]]$Diff.Time
              + T10[[b]]$Diff.Time)/(B*5)
              )
}
power_FGM
Time_FGM
