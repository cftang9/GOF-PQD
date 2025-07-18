rm(list=ls(all=TRUE))
library(xtable)

load("Sim_Rt_Deg_1.rdata")
T001 = Test_b
load("Sim_Rt_Deg_2.rdata")
T002 = Test_b
load("Sim_Rt_Deg_3.rdata")
T003 = Test_b
load("Sim_Rt_Deg_4.rdata")
T004 = Test_b
load("Sim_Rt_Deg_5.rdata")
T005 = Test_b
load("Sim_Rt_Deg_10.rdata")
T010 = Test_b
load("Sim_Rt_Deg_50.rdata")
T050 = Test_b
load("Sim_Rt_Deg_100.rdata")
T100 = Test_b



B = 1000

power_Rt = array(0,8); 
Time_Rt = 0; 
for(b in 1:B){
  power_Rt[1] = T001[[b]]$Rejection/B + power_Rt[1]
  power_Rt[2] = T002[[b]]$Rejection/B + power_Rt[2]
  power_Rt[3] = T003[[b]]$Rejection/B + power_Rt[3]
  power_Rt[4] = T004[[b]]$Rejection/B + power_Rt[4]
  power_Rt[5] = T005[[b]]$Rejection/B + power_Rt[5]
  
  power_Rt[6] = T010[[b]]$Rejection/B + power_Rt[6]
  power_Rt[7] = T050[[b]]$Rejection/B + power_Rt[7]
  power_Rt[8] = T100[[b]]$Rejection/B + power_Rt[8]
  
  Time_Rt = (Time_Rt 
             + (T001[[b]]$Diff.Time 
                + T002[[b]]$Diff.Time
                + T003[[b]]$Diff.Time
                + T004[[b]]$Diff.Time
                + T005[[b]]$Diff.Time
                + T010[[b]]$Diff.Time
                + T050[[b]]$Diff.Time
                + T100[[b]]$Diff.Time
                )/(B*8)
  )
}
power_Rt
Time_Rt
