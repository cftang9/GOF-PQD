rm(list=ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
source("All_PQD_GOF_n200.r")

n = 200; 
B = 1000; 

Tau = c(0.2,0.4); 
Table_2_FGM_Power = array(0,c(2,length(Tau)))
Table_2_FGM_Time = array(0,c(2,length(Tau)))

start.time = Sys.time(); 
for(k in 1:length(Tau)){
  for(b in 1:B){
    set.seed(081624000+b)
    fgm.cop <- fgmCopula(Tau[k],dim=2)
    Data <- rCopula(n, fgm.cop)
    
    temp_EL_A = PQD_GOF_n200(Data,method="EL_A")
    temp_EL_F = PQD_GOF_n200(Data,method="EL_F")
    
    
    Table_2_FGM_Power[1,k] = temp_EL_A$Rejection + Table_2_FGM_Power[1,k]; 
    Table_2_FGM_Power[2,k] = temp_EL_F$Rejection + Table_2_FGM_Power[2,k]; 
    
    Table_2_FGM_Time[1] = temp_EL_A$Time + Table_2_FGM_Time[1]; 
    Table_2_FGM_Time[2] = temp_EL_F$Time + Table_2_FGM_Time[2]; 
    
    ave.time = (Sys.time() - start.time)/((k-1)*B + b); 
    if(b%%100==0){
      print(c(k,b))
      print(Sys.time()+( (length(Tau)-k)*B + (B-b) )*ave.time )
    }
  }
}

Table_2_FGM_Power_n200 = Table_2_FGM_Power/B
Table_2_FGM_Time_n200 = array(Table_2_FGM_Time[,1]/(B*length(Tau)),c(2,1))

# save(Table_2_FGM_Power_n200,
#      Table_2_FGM_Time_n200,
#      file="Table_2_FGM_n200.rdata")