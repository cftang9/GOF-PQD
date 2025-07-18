rm(list=ls(all=TRUE))
library(xtable)

load("Sim_ClaytonTauN000.rdata")
ClaytonTauN00 = Test_b
load("Sim_ClaytonTauN005.rdata")
ClaytonTauN01 = Test_b
load("Sim_ClaytonTauN010.rdata")
ClaytonTauN02 = Test_b
load("Sim_ClaytonTauN015.rdata")
ClaytonTauN03 = Test_b
load("Sim_ClaytonTauN020.rdata")
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


load("Sim_FrankTauN000.rdata")
FrankTauN00 = Test_b
load("Sim_FrankTauN005.rdata")
FrankTauN01 = Test_b
load("Sim_FrankTauN010.rdata")
FrankTauN02 = Test_b
load("Sim_FrankTauN015.rdata")
FrankTauN03 = Test_b
load("Sim_FrankTauN020.rdata")
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


load("Sim_GumbelTauN000.rdata")
GumbelTauN00 = Test_b
load("Sim_GumbelTauN005.rdata")
GumbelTauN01 = Test_b
load("Sim_GumbelTauN010.rdata")
GumbelTauN02 = Test_b
load("Sim_GumbelTauN015.rdata")
GumbelTauN03 = Test_b
load("Sim_GumbelTauN020.rdata")
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


load("Sim_NormalRhoN000.rdata")
NormalRhoN00 = Test_b
load("Sim_NormalRhoN005.rdata")
NormalRhoN01 = Test_b
load("Sim_NormalRhoN010.rdata")
NormalRhoN02 = Test_b
load("Sim_NormalRhoN015.rdata")
NormalRhoN03 = Test_b
load("Sim_NormalRhoN020.rdata")
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