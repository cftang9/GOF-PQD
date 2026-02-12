rm(list=ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
source("All_PQD_GOF_n200.r")

n = 200; 
B = 1000; 

Rho = c(0.1,0.2); 
Table_1_Normal_Power = array(0,c(2,length(Rho)))
Table_1_Normal_Time = array(0,c(2,length(Rho)))

start.time = Sys.time(); 
for(k in 1:length(Rho)){
  for(b in 1:B){
    set.seed(081624000+b)
    cop = normalCopula(Rho[k])
    Data = rCopula(n,cop)
    
    temp_EL_A = PQD_GOF_n200(Data,method="EL_A")
    temp_EL_F = PQD_GOF_n200(Data,method="EL_F")
    
    Table_1_Normal_Power[1,k] = temp_EL_A$Rejection + Table_1_Normal_Power[1,k]; 
    Table_1_Normal_Power[2,k] = temp_EL_F$Rejection + Table_1_Normal_Power[2,k]; 
    
    Table_1_Normal_Time[1] = temp_EL_A$Time + Table_1_Normal_Time[1]; 
    Table_1_Normal_Time[2] = temp_EL_F$Time + Table_1_Normal_Time[2]; 
    
    ave.time = (Sys.time() - start.time)/((k-1)*B + b); 
    if(b%%100==0){
      print(c(k,b))
      print(Sys.time()+( (length(Rho)-k)*B + (B-b) )*ave.time )
    }
  }
}

Table_1_Normal_Power_n200 = Table_1_Normal_Power/B
Table_1_Normal_Time_n200 = array(Table_1_Normal_Time[,1]/(B*length(Rho)),c(2,1))

# save(Table_1_Normal_Power_n200, 
#      Table_1_Normal_Time_n200, 
#      file="Table_1_Normal_n200.rdata")