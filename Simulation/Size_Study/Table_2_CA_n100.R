rm(list=ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
source("Table_2_Generating_Data.r")
source("All_PQD_GOF_n100.r")

n = 100; 
B = 1000; 

Theta = c(0.1,0.2); 
Table_2_CA_Power = array(0,c(2,length(Theta)))
Table_2_CA_Time = array(0,c(2,length(Theta)))

start.time = Sys.time(); 
for(k in 1:length(Theta)){
  for(b in 1:B){
    set.seed(081624000+b)
    Data = rCA_coupla(n,theta=Theta[k])
    #Data[,1] = 1-Data[,1]; 
    
    temp_EL_A = PQD_GOF_n100(Data,method="EL_A")
    temp_EL_F = PQD_GOF_n100(Data,method="EL_F")
    
    Table_2_CA_Power[1,k] = temp_EL_A$Rejection + Table_2_CA_Power[1,k]; 
    Table_2_CA_Power[2,k] = temp_EL_F$Rejection + Table_2_CA_Power[2,k]; 
    
    Table_2_CA_Time[1] = temp_EL_A$Time + Table_2_CA_Time[1]; 
    Table_2_CA_Time[2] = temp_EL_F$Time + Table_2_CA_Time[2]; 
    
    ave.time = (Sys.time() - start.time)/((k-1)*B + b); 
    if(b%%100==0){
      print(c(k,b))
      print(Sys.time()+( (length(Theta)-k)*B + (B-b) )*ave.time )
    }
  }
}

Table_2_CA_Power_n100 = Table_2_CA_Power/B
Table_2_CA_Time_n100 = array(Table_2_CA_Time[,1]/(B*length(Theta)),c(2,1))

# save(Table_2_CA_Power_n100,
#      Table_2_CA_Time_n100,
#      file="Table_2_CA_n100.rdata")