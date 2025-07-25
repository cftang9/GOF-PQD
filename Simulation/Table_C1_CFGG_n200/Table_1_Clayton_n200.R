rm(list=ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
source("All_PQD_GOF_n200.r")

n = 200; 
B = 1000; 

Tau = c(0,-0.05,-0.10,-0.15,-0.20); 
Table_1_Clayton_Power = array(0,c(7,length(Tau)))
Table_1_Clayton_Time = array(0,c(7,length(Tau)))

start.time = Sys.time(); 
for(k in 1:length(Tau)){
  for(b in 1:B){
    set.seed(081624000+b)
    cop = claytonCopula(iTau(claytonCopula(),Tau[k]))
    Data = rCopula(n,cop)
    
    temp_EL_A = PQD_GOF_n200(Data,method="EL_A")
    temp_EL_F = PQD_GOF_n200(Data,method="EL_F")
    temp_DS04 = PQD_GOF_n200(Data,method="DS-04")
    temp_S05 = PQD_GOF_n200(Data,method="S-05")
    temp_EAD = PQD_GOF_n200(Data,method="EAD")
    temp_G10 = PQD_GOF_n200(Data,method="G-10")
    temp_LW14 = PQD_GOF_n200(Data,method="LW-14")
    
    Table_1_Clayton_Power[1,k] = temp_EL_A$Rejection + Table_1_Clayton_Power[1,k]; 
    Table_1_Clayton_Power[2,k] = temp_EL_F$Rejection + Table_1_Clayton_Power[2,k]; 
    Table_1_Clayton_Power[3,k] = temp_DS04$Rejection + Table_1_Clayton_Power[3,k]; 
    Table_1_Clayton_Power[4,k] = temp_S05$Rejection  + Table_1_Clayton_Power[4,k]; 
    Table_1_Clayton_Power[5,k] = temp_EAD$Rejection  + Table_1_Clayton_Power[5,k]; 
    Table_1_Clayton_Power[6,k] = temp_G10$Rejection  + Table_1_Clayton_Power[6,k]; 
    Table_1_Clayton_Power[7,k] = temp_LW14$Rejection + Table_1_Clayton_Power[7,k]; 
    
    Table_1_Clayton_Time[1] = temp_EL_A$Time + Table_1_Clayton_Time[1]; 
    Table_1_Clayton_Time[2] = temp_EL_F$Time + Table_1_Clayton_Time[2]; 
    Table_1_Clayton_Time[3] = temp_DS04$Time + Table_1_Clayton_Time[3]; 
    Table_1_Clayton_Time[4] = temp_S05$Time  + Table_1_Clayton_Time[4]; 
    Table_1_Clayton_Time[5] = temp_EAD$Time  + Table_1_Clayton_Time[5]; 
    Table_1_Clayton_Time[6] = temp_G10$Time  + Table_1_Clayton_Time[6]; 
    Table_1_Clayton_Time[7] = temp_LW14$Time + Table_1_Clayton_Time[7]; 
    
    ave.time = (Sys.time() - start.time)/((k-1)*B + b); 
    if(b%%100==0){
      print(c(k,b))
      print(Sys.time()+( (length(Tau)-k)*B + (B-b) )*ave.time )
    }
  }
}

Table_1_Clayton_Power_n200 = Table_1_Clayton_Power/B
Table_1_Clayton_Time_n200 = array(Table_1_Clayton_Time[,1]/(B*length(Tau)),c(7,1))

# save(Table_1_Clayton_Power_n200, 
#      Table_1_Clayton_Time_n200, 
#      file="Table_1_Clayton_n200.rdata")