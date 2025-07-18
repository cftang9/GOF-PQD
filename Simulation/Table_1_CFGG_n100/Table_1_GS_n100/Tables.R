rm(list=ls(all=TRUE))
library(xtable)

load("Sim_ClaytonTauN00.rdata")
ClaytonTauN00 = Test_b
load("Sim_ClaytonTauN01.rdata")
ClaytonTauN01 = Test_b
load("Sim_ClaytonTauN02.rdata")
ClaytonTauN02 = Test_b
load("Sim_ClaytonTauN03.rdata")
ClaytonTauN03 = Test_b
load("Sim_ClaytonTauN04.rdata")
ClaytonTauN04 = Test_b

B = 1000; 
ClaytonTable = array(0,6)
for(k in 0:4){
  for(b in 1:B){
    ClaytonTable[k+1] = 
      (get(paste0("ClaytonTauN0",k))[[b]]$Rejection/B 
       + ClaytonTable[k+1] )
    ClaytonTable[6] = 
      (get(paste0("ClaytonTauN0",k))[[b]]$Diff.Time/(B*5) 
       + ClaytonTable[6] )
  }
}
ClaytonTable


load("Sim_FrankTauN00.rdata")
FrankTauN00 = Test_b
load("Sim_FrankTauN01.rdata")
FrankTauN01 = Test_b
load("Sim_FrankTauN02.rdata")
FrankTauN02 = Test_b
load("Sim_FrankTauN03.rdata")
FrankTauN03 = Test_b
load("Sim_FrankTauN04.rdata")
FrankTauN04 = Test_b

B = 1000; 
FrankTable = array(0,6)
for(k in 0:4){
  for(b in 1:B){
    FrankTable[k+1] = 
      (get(paste0("FrankTauN0",k))[[b]]$Rejection/B 
       + FrankTable[k+1] )
    FrankTable[6] = 
      (get(paste0("FrankTauN0",k))[[b]]$Diff.Time/(B*5) 
       + FrankTable[6] )
  }
}
FrankTable


load("Sim_GumbelTauN00.rdata")
GumbelTauN00 = Test_b
load("Sim_GumbelTauN01.rdata")
GumbelTauN01 = Test_b
load("Sim_GumbelTauN02.rdata")
GumbelTauN02 = Test_b
load("Sim_GumbelTauN03.rdata")
GumbelTauN03 = Test_b
load("Sim_GumbelTauN04.rdata")
GumbelTauN04 = Test_b

B = 1000; 
GumbelTable = array(0,6)
for(k in 0:4){
  for(b in 1:B){
    GumbelTable[k+1] = 
      (get(paste0("GumbelTauN0",k))[[b]]$Rejection/B 
       + GumbelTable[k+1] )
    GumbelTable[6] = 
      (get(paste0("GumbelTauN0",k))[[b]]$Diff.Time/(B*5) 
       + GumbelTable[6] )
  }
}
GumbelTable


load("Sim_NormalRhoN00.rdata")
NormalRhoN00 = Test_b
load("Sim_NormalRhoN01.rdata")
NormalRhoN01 = Test_b
load("Sim_NormalRhoN02.rdata")
NormalRhoN02 = Test_b
load("Sim_NormalRhoN03.rdata")
NormalRhoN03 = Test_b
load("Sim_NormalRhoN04.rdata")
NormalRhoN04 = Test_b

B = 1000; 
NormalTable = array(0,6)
for(k in 0:4){
  for(b in 1:B){
    NormalTable[k+1] = 
      (get(paste0("NormalRhoN0",k))[[b]]$Rejection/B 
       + NormalTable[k+1] )
    NormalTable[6] = 
      (get(paste0("NormalRhoN0",k))[[b]]$Diff.Time/(B*5) 
       + NormalTable[6] )
  }
}
NormalTable

xtable(rbind(
  ClaytonTable, 
  FrankTable,
  GumbelTable,
  NormalTable
),digit=3)