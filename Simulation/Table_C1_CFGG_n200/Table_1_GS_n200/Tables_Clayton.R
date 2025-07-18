rm(list=ls(all=TRUE))

load("Sim_ClaytonTauN005.rdata")
NT00 = Test_b
load("Sim_ClaytonTauN010.rdata")
NT01 = Test_b
load("Sim_ClaytonTauN015.rdata")
NT02 = Test_b
load("Sim_ClaytonTauN020.rdata")
NT03 = Test_b
load("Sim_ClaytonTauN025.rdata")
NT04 = Test_b

B = 1000
Table = array(0,6)
for(k in 0:4){
  for(i in 1:B){
    Table[k+1] = get(paste0("NT0",k))[[i]]$Rejection/B + Table[k+1]
    Table[6] = get(paste0("NT0",k))[[i]]$Diff.Time/B/5 + Table[6]
  }
}

Table