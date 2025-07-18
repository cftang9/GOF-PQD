rm(list = ls(all=TRUE))
library(parallel)
library(foreach)
library(doParallel)
library(copula)
library(Matrix)
source("DS2004.R")


n = 200; 
B = 1000; 

do_b = function(b){
  set.seed(081624000+b)
  Data = array(runif(2*n),c(n,2))
  Ind = c(Data[,1]>=0.15 & Data[,1]<=0.5 & Data[,2]>=0.5 & Data[,2]<=0.85)
  Data[Ind, ] =  cbind( 0.5 - (Data[Ind, 1]-0.15) , 0.5 + (Data[Ind, 1]-0.15))
  
  
  Start.Time = Sys.time()
  Start.Time.DS = Sys.time(); 
  DS = DS_C(Data)
  Duration.Time.DS = difftime(Sys.time(), Start.Time.DS, units='sec')
  write.table(data.frame(b=b, Start.Time = Start.Time, Current.Time=Sys.time(), 
                         Duration.Time.DS = Duration.Time.DS, DS.Rejection = as.numeric(DS$Reject), DS.TS = DS$TestStatistic, 
                         cores=detectCores()), file = "Sim_Copula_A.csv", append = TRUE,
              row.names = FALSE, col.names = !file.exists("Sim_Copula_A.csv"),
              sep = ",")
  return(NULL)
}

numCores <- detectCores() - 1 
cl <- makeCluster(numCores)
clusterExport(cl, varlist = c("do_b"))
registerDoParallel(cl)
clusterEvalQ(cl, 
             {
               library(parallel)
               library(foreach)
               library(doParallel)
               library(copula)
               library(matrixcalc)}
)
Test_b = foreach(b=1:B, 
                 .options.scheduling = "dynamic") %dopar% {do_b(b)}
stopCluster(cl)
