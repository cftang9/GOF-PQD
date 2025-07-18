rm(list=ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
source("All_PQD_GOF_n200.r")
source("Table_2_Generating_Data.r")

n = 200; 
B = 1000; 

Case = c("Mix_Frank_1","Mix_Frank_2",
         "Asymmetric_Copula","Copula_A", 
         "qrm","srm"); 
Table_2_6C_Power = array(0,c(7,length(Case)))
Table_2_6C_Time = array(0,c(7,length(Case)))

start.time = Sys.time(); 
for(k in 1:length(Case)){
  for(b in 1:B){
    #set.seed(081624000+b)
    Data = r6Copulas(n=n,b=b,Copula=Case[k])
    
    temp_EL_A = PQD_GOF_n200(Data,method="EL_A")
    temp_EL_F = PQD_GOF_n200(Data,method="EL_F")
    temp_DS04 = PQD_GOF_n200(Data,method="DS-04")
    temp_S05 = PQD_GOF_n200(Data,method="S-05")
    temp_EAD = PQD_GOF_n200(Data,method="EAD")
    temp_G10 = PQD_GOF_n200(Data,method="G-10")
    temp_LW14 = PQD_GOF_n200(Data,method="LW-14")
    
    Table_2_6C_Power[1,k] = temp_EL_A$Rejection + Table_2_6C_Power[1,k]; 
    Table_2_6C_Power[2,k] = temp_EL_F$Rejection + Table_2_6C_Power[2,k]; 
    Table_2_6C_Power[3,k] = temp_DS04$Rejection + Table_2_6C_Power[3,k]; 
    Table_2_6C_Power[4,k] = temp_S05$Rejection  + Table_2_6C_Power[4,k]; 
    Table_2_6C_Power[5,k] = temp_EAD$Rejection  + Table_2_6C_Power[5,k]; 
    Table_2_6C_Power[6,k] = temp_G10$Rejection  + Table_2_6C_Power[6,k]; 
    Table_2_6C_Power[7,k] = temp_LW14$Rejection + Table_2_6C_Power[7,k]; 
    
    Table_2_6C_Time[1,k] = temp_EL_A$Time + Table_2_6C_Time[1,k]; 
    Table_2_6C_Time[2,k] = temp_EL_F$Time + Table_2_6C_Time[2,k]; 
    Table_2_6C_Time[3,k] = temp_DS04$Time + Table_2_6C_Time[3,k]; 
    Table_2_6C_Time[4,k] = temp_S05$Time  + Table_2_6C_Time[4,k]; 
    Table_2_6C_Time[5,k] = temp_EAD$Time  + Table_2_6C_Time[5,k]; 
    Table_2_6C_Time[6,k] = temp_G10$Time  + Table_2_6C_Time[6,k]; 
    Table_2_6C_Time[7,k] = temp_LW14$Time + Table_2_6C_Time[7,k]; 
    
    ave.time = (Sys.time() - start.time)/((k-1)*B + b); 
    if(b%%100==0){
      print(c(k,b))
      print(Sys.time()+( (length(Case)-k)*B + (B-b) )*ave.time )
    }
  }
}

Table_2_6C_Power_n200 = Table_2_6C_Power/B
Table_2_6C_Time_n200 = Table_2_6C_Time/B

# save(Table_2_6C_Power_n200,
#      Table_2_6C_Time_n200,
#      file="Table_2_6C_n200.rdata")