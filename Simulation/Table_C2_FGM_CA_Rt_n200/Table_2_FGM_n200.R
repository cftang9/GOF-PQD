rm(list=ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
source("All_PQD_GOF_n200.r")

n = 200; 
B = 1000; 

Tau = c(-0.2,-0.4,-0.6,-0.8,-1); 
Table_2_FGM_Power = array(0,c(7,length(Tau)))
Table_2_FGM_Time = array(0,c(7,length(Tau)))

start.time = Sys.time(); 
for(k in 1:length(Tau)){
  for(b in 1:B){
    library(copula)
    fgm.cop <- fgmCopula(Tau[k],dim=2)
    Data <- rCopula(n, fgm.cop)
    
    temp_EL_A = PQD_GOF_n200(Data,method="EL_A")
    temp_EL_F = PQD_GOF_n200(Data,method="EL_F")
    temp_DS04 = PQD_GOF_n200(Data,method="DS-04")
    temp_S05 = PQD_GOF_n200(Data,method="S-05")
    temp_EAD = PQD_GOF_n200(Data,method="EAD")
    temp_G10 = PQD_GOF_n200(Data,method="G-10")
    temp_LW14 = PQD_GOF_n200(Data,method="LW-14")
    
    Table_2_FGM_Power[1,k] = temp_EL_A$Rejection + Table_2_FGM_Power[1,k]; 
    Table_2_FGM_Power[2,k] = temp_EL_F$Rejection + Table_2_FGM_Power[2,k]; 
    Table_2_FGM_Power[3,k] = temp_DS04$Rejection + Table_2_FGM_Power[3,k]; 
    Table_2_FGM_Power[4,k] = temp_S05$Rejection  + Table_2_FGM_Power[4,k]; 
    Table_2_FGM_Power[5,k] = temp_EAD$Rejection  + Table_2_FGM_Power[5,k]; 
    Table_2_FGM_Power[6,k] = temp_G10$Rejection  + Table_2_FGM_Power[6,k]; 
    Table_2_FGM_Power[7,k] = temp_LW14$Rejection + Table_2_FGM_Power[7,k]; 
    
    Table_2_FGM_Time[1] = temp_EL_A$Time + Table_2_FGM_Time[1]; 
    Table_2_FGM_Time[2] = temp_EL_F$Time + Table_2_FGM_Time[2]; 
    Table_2_FGM_Time[3] = temp_DS04$Time + Table_2_FGM_Time[3]; 
    Table_2_FGM_Time[4] = temp_S05$Time  + Table_2_FGM_Time[4]; 
    Table_2_FGM_Time[5] = temp_EAD$Time  + Table_2_FGM_Time[5]; 
    Table_2_FGM_Time[6] = temp_G10$Time  + Table_2_FGM_Time[6]; 
    Table_2_FGM_Time[7] = temp_LW14$Time + Table_2_FGM_Time[7]; 
    
    ave.time = (Sys.time() - start.time)/((k-1)*B + b); 
    if(b%%100==0){
      print(c(k,b))
      print(Sys.time()+( (length(Tau)-k)*B + (B-b) )*ave.time )
    }
  }
}

Table_2_FGM_Power_n200 = Table_2_FGM_Power/B
Table_2_FGM_Time_n200 = array(Table_2_FGM_Time[,1]/(B*length(Tau)),c(7,1))

# save(Table_2_FGM_Power_n200,
#      Table_2_FGM_Time_n200,
#      file="Table_2_FGM_n200.rdata")