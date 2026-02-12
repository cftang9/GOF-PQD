rm(list=ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
source("All_PQD_GOF_n100.r")

n = 100; 
B = 1000; 

Tau = c(0.1,0.2); 
Table_1_Gumbel_Power = array(0,c(2,length(Tau)))
Table_1_Gumbel_Time = array(0,c(2,length(Tau)))

start.time = Sys.time(); 
for(k in 1:length(Tau)){
  for(b in 1:B){
    set.seed(081624000+b)
    cop = gumbelCopula(iTau(gumbelCopula(),Tau[k]))
    Data = rCopula(n,cop)
    #Data = cbind(Data[,1],1-Data[,2])
    
    temp_EL_A = PQD_GOF_n100(Data,method="EL_A")
    temp_EL_F = PQD_GOF_n100(Data,method="EL_F")
    
    Table_1_Gumbel_Power[1,k] = temp_EL_A$Rejection + Table_1_Gumbel_Power[1,k]; 
    Table_1_Gumbel_Power[2,k] = temp_EL_F$Rejection + Table_1_Gumbel_Power[2,k]; 
    
    Table_1_Gumbel_Time[1] = temp_EL_A$Time + Table_1_Gumbel_Time[1]; 
    Table_1_Gumbel_Time[2] = temp_EL_F$Time + Table_1_Gumbel_Time[2]; 
    
    ave.time = (Sys.time() - start.time)/((k-1)*B + b); 
    if(b%%100==0){
      print(c(k,b))
      print(Sys.time()+( (length(Tau)-k)*B + (B-b) )*ave.time )
    }
  }
}

Table_1_Gumbel_Power_n100 = Table_1_Gumbel_Power/B
Table_1_Gumbel_Time_n100 = array(Table_1_Gumbel_Time[,1]/(B*length(Tau)),c(2,1))


# save(Table_1_Gumbel_Power_n100, 
#      Table_1_Gumbel_Time_n100, 
#      file="Table_1_Gumbel_n100.rdata")