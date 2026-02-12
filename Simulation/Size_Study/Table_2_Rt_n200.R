rm(list=ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
library(mvtnorm)
source("All_PQD_GOF_n200.r")

n = 200; 
B = 1000; 

Nu = c(2,5); 
Table_2_Rt_Power = array(0,c(2,length(Nu)))
Table_2_Rt_Time = array(0,c(2,length(Nu)))

start.time = Sys.time(); 
for(k in 1:length(Nu)){
  for(b in 1:B){
    set.seed(081624000+b)
    Data = rmvt(n,df=Nu[k]); 
    Data = abs(Data)
    #Data[,1] = -Data[,1]
    
    temp_EL_A = PQD_GOF_n200(Data,method="EL_A")
    temp_EL_F = PQD_GOF_n200(Data,method="EL_F")
    
    Table_2_Rt_Power[1,k] = temp_EL_A$Rejection + Table_2_Rt_Power[1,k]; 
    Table_2_Rt_Power[2,k] = temp_EL_F$Rejection + Table_2_Rt_Power[2,k]; 
    
    Table_2_Rt_Time[1] = temp_EL_A$Time + Table_2_Rt_Time[1]; 
    Table_2_Rt_Time[2] = temp_EL_F$Time + Table_2_Rt_Time[2]; 
    
    ave.time = (Sys.time() - start.time)/((k-1)*B + b); 
    if(b%%100==0){
      print(c(k,b))
      print(Sys.time()+( (length(Nu)-k)*B + (B-b) )*ave.time )
    }
  }
}

Table_2_Rt_Power_n200 = Table_2_Rt_Power/B
Table_2_Rt_Time_n200 = array(Table_2_Rt_Time[,1]/(B*length(Nu)),c(2,1))

# save(Table_2_Rt_Power_n200,
#      Table_2_Rt_Time_n200,
#      file="Table_2_Rt_n200.rdata")