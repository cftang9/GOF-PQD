rm(list=ls(all=TRUE))
library(xtable)

load("Sim_FGM_N02.rdata")
FGM_1 = Test_b
load("Sim_FGM_N04.rdata")
FGM_2 = Test_b
load("Sim_FGM_N06.rdata")
FGM_3 = Test_b
load("Sim_FGM_N08.rdata")
FGM_4 = Test_b
load("Sim_FGM_N10.rdata")
FGM_5 = Test_b

B = 1000; 
FGM_Table = array(0,6)
for(k in 1:5){
  for(b in 1:B){
    FGM_Table[k] = 
      (get(paste0("FGM_",k))[[b]]$Rejection/B 
       + FGM_Table[k] )
    FGM_Table[6] = 
      (get(paste0("FGM_",k))[[b]]$Diff.Time/(B*5) 
       + FGM_Table[6] )
  }
}
FGM_Table

load("Sim_CA_N005.rdata")
CA_1 = Test_b
load("Sim_CA_N010.rdata")
CA_2 = Test_b
load("Sim_CA_N015.rdata")
CA_3 = Test_b
load("Sim_CA_N020.rdata")
CA_4 = Test_b
load("Sim_CA_N025.rdata")
CA_5 = Test_b

B = 1000; 
CA_Table = array(0,6)
for(k in 1:5){
  for(b in 1:B){
    CA_Table[k] = 
      (get(paste0("CA_",k))[[b]]$Rejection/B 
       + CA_Table[k] )
    CA_Table[6] = 
      (get(paste0("CA_",k))[[b]]$Diff.Time/(B*5) 
       + CA_Table[6] )
  }
}
CA_Table

load("Sim_Rt_Deg_1.rdata")
Rt_1 = Test_b
load("Sim_Rt_Deg_2.rdata")
Rt_2 = Test_b
load("Sim_Rt_Deg_3.rdata")
Rt_3 = Test_b
load("Sim_Rt_Deg_4.rdata")
Rt_4 = Test_b
load("Sim_Rt_Deg_5.rdata")
Rt_5 = Test_b
load("Sim_Rt_Deg_100.rdata")
Rt_6 = Test_b

B = 1000; 
Rt_Table = array(0,7)
for(k in 1:6){
  for(b in 1:B){
    Rt_Table[k] = 
      (get(paste0("Rt_",k))[[b]]$Rejection/B 
       + Rt_Table[k] )
    Rt_Table[7] = 
      (get(paste0("Rt_",k))[[b]]$Diff.Time/(B*6) 
       + Rt_Table[7] )
  }
}
Rt_Table

xtable(rbind(
  FGM_Table,
  CA_Table
),digit=3)

xtable(data.frame(t(Rt_Table)),digit=3)